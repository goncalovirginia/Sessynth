open Sexplib;;
open Sexplib.Sexp;;
open Language;;

exception CVC5Error of string 
exception CVC5ParseError of string
let sygus_code1 = {|
(set-logic NIA)
(synth-fun |}

let sygus_code2 = {|
  ((Start Int) (StartBool Bool))
  ((Start Int (0 1 |}

let sygus_code3 = {|
               (+ Start Start)
               (- Start Start)
               (* Start Start)
               (div Start Start)
               (ite StartBool Start Start)))
   (StartBool Bool ((and StartBool StartBool)
                    (or StartBool StartBool)
                    (not StartBool)
                    (< Start Start)
                    (<= Start Start)
                    (= Start Start)
                    (> Start Start)
                    (>= Start Start)))))
|}

let sygus_code4 = {|
(check-synth)
|}

let call_cvc5 sygus_code =
  let command = "cvc5 --lang=sygus2" in
  let (in_ch, out_ch, err_ch) = Unix.open_process_full command (Unix.environment ()) in
  output_string out_ch sygus_code;
  flush out_ch;
  close_out out_ch;

  let buf = Buffer.create 1024 in
  (try
     while true do
       let line = input_line in_ch in
       Buffer.add_string buf line;
       Buffer.add_char buf '\n';
     done
   with End_of_file -> ());
  close_in in_ch;
  close_in err_ch;
  Buffer.contents buf

let rec parse_sexp sexp =
 	match sexp with
  	| Atom x ->
    	begin try Int(int_of_string x)
     	with Failure _ ->
       		match x with
       		| "true" -> Bool(true)
       		| "false" -> Bool(false)
       		| _ -> Var(x)
     	end
	| List [Atom "and"; a; b] -> And(parse_sexp a, parse_sexp b)
  	| List [Atom "or"; a; b] -> Or(parse_sexp a, parse_sexp b)
  	| List [Atom "="; a; b] -> Eq(parse_sexp a, parse_sexp b)
  	| List [Atom ">"; a; b] -> Gr(parse_sexp a, parse_sexp b)
  	| List [Atom "<"; a; b] -> Lt(parse_sexp a, parse_sexp b)
  	| List [Atom ">="; a; b] -> GrE(parse_sexp a, parse_sexp b)
  	| List [Atom "<="; a; b] -> LtE(parse_sexp a, parse_sexp b)
  	| List [Atom "+"; a; b] -> Sum(parse_sexp a, parse_sexp b)
  	| List [Atom "-"; a; b] -> Sub(parse_sexp a, parse_sexp b)
  	| List [Atom "*"; a; b] -> Mult(parse_sexp a, parse_sexp b)
  	| List [Atom "div"; a; b] -> Div(parse_sexp a, parse_sexp b)
  	| List [Atom "ite"; c; t; e] -> Ite(parse_sexp c, parse_sexp t, parse_sexp e)
  	| List [Atom "define-fun"; Atom name; List params; Atom _rtype; body] -> parse_sexp body
  	| _ -> raise (CVC5ParseError ("Unsupported expression: " ^ Sexp.to_string_hum sexp))

let parse_cvc5_output output =
	let sexps = Sexp.scan_sexps (Lexing.from_string output) in
  	match sexps with
  	| [List [define_fun]] -> [parse_sexp define_fun]
  	| [List defs] -> List.map parse_sexp defs
  	| _ -> raise (CVC5ParseError "Unexpected output format from CVC5")

let tyR_to_sygus_constraint tR =	
	let rec tyR_to_sygus_constraint' tR =
  		match tR with
  		| RTAnd(a, b) -> Printf.sprintf "(and %s %s)" (tyR_to_sygus_constraint' a) (tyR_to_sygus_constraint' b)
  		| RTOr(a, b) -> Printf.sprintf "(or %s %s)" (tyR_to_sygus_constraint' a) (tyR_to_sygus_constraint' b)
  		| RTEq(a, b) -> Printf.sprintf "(= %s %s)" (tyR_to_sygus_constraint' a) (tyR_to_sygus_constraint' b)
  		| RTGr(a, b) -> Printf.sprintf "(> %s %s)" (tyR_to_sygus_constraint' a) (tyR_to_sygus_constraint' b)
  		| RTLt(a, b) -> Printf.sprintf "(< %s %s)" (tyR_to_sygus_constraint' a) (tyR_to_sygus_constraint' b)
  		| RTGrE(a, b) -> Printf.sprintf "(>= %s %s)" (tyR_to_sygus_constraint' a) (tyR_to_sygus_constraint' b)
  		| RTLtE(a, b) -> Printf.sprintf "(<= %s %s)" (tyR_to_sygus_constraint' a) (tyR_to_sygus_constraint' b)
  		| RTSum(a, b) -> Printf.sprintf "(+ %s %s)" (tyR_to_sygus_constraint' a) (tyR_to_sygus_constraint' b)
  		| RTSub(a, b) -> Printf.sprintf "(- %s %s)" (tyR_to_sygus_constraint' a) (tyR_to_sygus_constraint' b)
  		| RTMult(a, b) -> Printf.sprintf "(* %s %s)" (tyR_to_sygus_constraint' a) (tyR_to_sygus_constraint' b)
  		| RTDiv(a, b) -> Printf.sprintf "(div %s %s)" (tyR_to_sygus_constraint' a) (tyR_to_sygus_constraint' b)
  		| RTInt(n) -> string_of_int n
  		| RTBool(true) -> "true"
  		| RTBool(false) -> "false"
  		| RTVar(x) -> x
	in
	Printf.sprintf "(constraint %s)" (tyR_to_sygus_constraint' tR)

type parsed_TRefinement = { x : id; tA : id; constr : id }

let tyA_to_sygus = function
	| TInt -> "Int"
	| TBool -> "Bool"

let parse_tyF x t =
	match t with
	| TAtomic(tA) -> Some { x = x; tA = tyA_to_sygus tA; constr = ""}
	| TRefinement(x, tA, tR) -> 
		begin match tR with 
		| RTBool _ -> Some { x = x; tA = tyA_to_sygus tA; constr = ""}
		| _ -> Some { x = x; tA = tyA_to_sygus tA; constr = tyR_to_sygus_constraint tR }
		end
	| _ -> None

let format_function_to_sygus ps_sygus goal_sygus =
	let rec append_args ps_sygus' =
		match ps_sygus' with
		| [] -> ")"
		| p::ps_sygus'' -> " " ^ p.x ^ append_args ps_sygus'' in
	let formatted = "(" ^ goal_sygus.x ^ append_args ps_sygus in
	formatted

let replace_occurences s target replacement =
	let re = Str.regexp_string target in
	Str.global_replace re replacement s

let build_sygus_input ps goal =
	let ps_sygus = List.filter_map ( fun (x, t) -> parse_tyF x t ) ps in
	let goal_sygus = Option.get (parse_tyF "" goal) in
	let sygus_input = sygus_code1 ^ goal_sygus.x ^ " (" in
	let rec append_args ps_sygus' =
		match ps_sygus' with
		| [] -> ""
		| [p] -> Printf.sprintf "(%s %s))" p.x p.tA
		| p::ps_sygus'' -> Printf.sprintf "(%s %s) " p.x p.tA ^ append_args ps_sygus'' in
	let sygus_input = sygus_input ^ append_args ps_sygus in
	let sygus_input = sygus_input ^ " " ^ goal_sygus.tA ^ sygus_code2 in
	let rec append_vars ps_sygus' =
	match ps_sygus' with
	| [] -> ""
	| p::ps_sygus'' -> p.x ^ " " ^ append_vars ps_sygus'' in
	let sygus_input = sygus_input ^ append_vars ps_sygus ^ sygus_code3 in
	let rec append_declare_vars ps_sygus' =
	match ps_sygus' with
	| [] -> ""
	| p::ps_sygus'' -> Printf.sprintf "(declare-var %s %s)\n" p.x p.tA ^ append_declare_vars ps_sygus'' in
	let sygus_input = sygus_input ^ append_declare_vars ps_sygus in
	let rec append_constraints ps_sygus' =
	match ps_sygus' with
	| [] -> ""
	| p::ps_sygus'' -> (if p.constr = "" then "" else p.constr ^ "\n") ^ append_constraints ps_sygus'' in
	let sygus_input = sygus_input ^ append_constraints ps_sygus in
	let formatted_function = format_function_to_sygus ps_sygus goal_sygus in
	let formatted_goal_sygus_constr = replace_occurences goal_sygus.constr goal_sygus.x formatted_function in
	let sygus_input = sygus_input ^ formatted_goal_sygus_constr ^ sygus_code4 in
	sygus_input

let solve ps goal =
	let sygus_input = build_sygus_input ps goal in
	print_endline sygus_input;
  	let sygus_output = call_cvc5 sygus_input in
  	print_endline sygus_output;
	List.hd (parse_cvc5_output sygus_output)
