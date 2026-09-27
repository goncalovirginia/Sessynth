open Sexplib;;
open Sexplib.Sexp;;
open Language;;

exception CVC5Error of string 
exception CVC5Infeasible of string 
exception CVC5ParseError of string
let sygus_code1 = {|
(set-logic NIA)
(synth-fun |}

let sygus_code2 = {|
  ((StartIte Int) (StartInt Int) (StartBool Bool))
  ((StartIte Int (StartInt
               (ite StartBool StartInt StartInt)))
   (StartInt Int (0 1 |}

let sygus_code3 = {|
               (+ StartInt StartInt)
               (- StartInt StartInt)
               (* StartInt StartInt)))
   (StartBool Bool ((and StartBool StartBool)
                    (or StartBool StartBool)
                    (not StartBool)
                    (< StartInt StartInt)
                    (<= StartInt StartInt)
                    (= StartInt StartInt)))))
|}

let sygus_code4 = {|
(check-synth)
|}

let call_sygus sygus_code =
	incr Flags.solver_calls;
  	let command = "cvc5 --lang=sygus2" in
  	let (in_ch, out_ch, err_ch) = Unix.open_process_full command (Unix.environment ()) in
  	output_string out_ch sygus_code;
  	flush out_ch;
  	close_out out_ch;

  	let buf = Buffer.create 1024 in
  	(try while true do
      	let line = input_line in_ch in
      	Buffer.add_string buf line;
      	Buffer.add_char buf '\n';
     	done
   	with End_of_file -> ());
  	close_in in_ch;
  	close_in err_ch;
  	Buffer.contents buf

let call_sat sat_code =
	incr Flags.solver_calls;
	let command = "cvc5 --lang=smt2" in
  	let (in_ch, out_ch, err_ch) = Unix.open_process_full command (Unix.environment ()) in
  	output_string out_ch sat_code;
  	flush out_ch;
  	close_out out_ch;
	let output = input_line in_ch in
  	close_in in_ch;
  	close_in err_ch;
	output

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
	| List [Atom "and"; a; b] -> BOp(And, parse_sexp a, parse_sexp b)
  	| List [Atom "or"; a; b] -> BOp(Or, parse_sexp a, parse_sexp b)
  	| List [Atom "="; a; b] -> BOp(Eq, parse_sexp a, parse_sexp b)
  	| List [Atom ">"; a; b] -> BOp(Gr, parse_sexp a, parse_sexp b)
  	| List [Atom "<"; a; b] -> BOp(Lt, parse_sexp a, parse_sexp b)
  	| List [Atom ">="; a; b] -> BOp(GrE, parse_sexp a, parse_sexp b)
  	| List [Atom "<="; a; b] -> BOp(LtE, parse_sexp a, parse_sexp b)
  	| List [Atom "+"; a; b] -> BOp(Sum, parse_sexp a, parse_sexp b)
  	| List [Atom "-"; a; b] -> BOp(Sub, parse_sexp a, parse_sexp b)
  	| List [Atom "*"; a; b] -> BOp(Mult, parse_sexp a, parse_sexp b)
  	| List [Atom "div"; a; b] -> BOp(Div, parse_sexp a, parse_sexp b)
  	| List [Atom "ite"; c; t; e] -> Ite(parse_sexp c, parse_sexp t, parse_sexp e)
  	| List [Atom "define-fun"; Atom _name; List _params; Atom _rtype; body] -> parse_sexp body
  	| _ -> raise (CVC5ParseError ("Unsupported expression: " ^ Sexp.to_string_hum sexp))

let contains_substring s sub =
  try ignore (Str.search_forward (Str.regexp_string sub) s 0); true
  with Not_found -> false

let parse_sygus_output output =
	if contains_substring output "infeasible" then raise (CVC5Infeasible "Goal function is infeasible") else
	let sexps = Sexp.scan_sexps (Lexing.from_string output) in
  	match sexps with
  	| [List [define_fun]] -> [parse_sexp define_fun]
  	| [List defs] -> List.map parse_sexp defs
  	| _ -> raise (CVC5ParseError "Unexpected output format from CVC5")
	
(** Renders [tR] as an s-expression. [render_var] decides how a variable
    occurrence is printed, and defaults to its own name; the goal's binder stands
    for an application of the function being synthesized, and substituting it
    here rather than over the finished string is what keeps a binder from also
    being replaced inside a longer name that happens to contain it. *)
let tyR_to_sexp_string ?(render_var = fun x -> x) tR =
  	let rec tyR_to_sexp_string tR =
  	match tR with
  	| RTBOp(And, a, b) -> Printf.sprintf "(and %s %s)" (tyR_to_sexp_string a) (tyR_to_sexp_string b)
  	| RTBOp(Or, a, b) -> Printf.sprintf "(or %s %s)" (tyR_to_sexp_string a) (tyR_to_sexp_string b)
  	| RTBOp(Eq, a, b) -> Printf.sprintf "(= %s %s)" (tyR_to_sexp_string a) (tyR_to_sexp_string b)
  	| RTBOp(Gr, a, b) -> Printf.sprintf "(> %s %s)" (tyR_to_sexp_string a) (tyR_to_sexp_string b)
  	| RTBOp(Lt, a, b) -> Printf.sprintf "(< %s %s)" (tyR_to_sexp_string a) (tyR_to_sexp_string b)
  	| RTBOp(GrE, a, b) -> Printf.sprintf "(>= %s %s)" (tyR_to_sexp_string a) (tyR_to_sexp_string b)
  	| RTBOp(LtE, a, b) -> Printf.sprintf "(<= %s %s)" (tyR_to_sexp_string a) (tyR_to_sexp_string b)
  	| RTBOp(Sum, a, b) -> Printf.sprintf "(+ %s %s)" (tyR_to_sexp_string a) (tyR_to_sexp_string b)
  	| RTBOp(Sub, a, b) -> Printf.sprintf "(- %s %s)" (tyR_to_sexp_string a) (tyR_to_sexp_string b)
  	| RTBOp(Mult, a, b) -> Printf.sprintf "(* %s %s)" (tyR_to_sexp_string a) (tyR_to_sexp_string b)
  	| RTBOp(Div, a, b) -> Printf.sprintf "(div %s %s)" (tyR_to_sexp_string a) (tyR_to_sexp_string b)
  	| RTUOp(Not, a) -> Printf.sprintf "(not %s)" (tyR_to_sexp_string a)
	| RTUOp(Neg, a) -> Printf.sprintf "(- %s)" (tyR_to_sexp_string a)
	| RTInt(n) -> if n < 0 
		then Printf.sprintf "(- %s)" (string_of_int (-n))
		else string_of_int n
  	| RTBool(true) -> "true"
  	| RTBool(false) -> "false"
  	| RTVar(x) -> render_var x
  	in tyR_to_sexp_string tR

(** [constr] is the predicate itself rather than its rendering, so that the goal's
   can still be substituted into before it is printed. [None] is a predicate that
   constrains nothing, which needs neither a hypothesis nor a proof obligation *)
type parsed_TRefinement = { x : id; tA : id; constr : tyR option }

let tyA_to_sygus = function
	| TInt -> "Int"
	| TBool -> "Bool"
	| TUnit | TPolyVar _ | TRigidVar _ -> assert false

let parse_tyF x t =
	match t with
	| TAtomic(TUnit) -> None
	| TAtomic(tA) -> Some { x = x; tA = tyA_to_sygus tA; constr = None }
	| TRefinement(x, tA, tR) ->
		begin match tR with
		| RTBool _ -> Some { x = x; tA = tyA_to_sygus tA; constr = None }
		| _ -> Some { x = x; tA = tyA_to_sygus tA; constr = Some tR }
		end
	| _ -> None

let format_function_to_sygus ps_sygus goal_sygus =
	let rec append_args ps_sygus' =
		match ps_sygus' with
		| [] -> ")"
		| p::ps_sygus'' -> " " ^ p.x ^ append_args ps_sygus'' in
	let formatted = "(" ^ goal_sygus.x ^ append_args ps_sygus in
	formatted

let get_TRefinement_tyR t =
	match t with
	| TRefinement(_, _, tR) -> tR
	| _ -> assert false

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
	(* the parameters' predicates are what may be assumed of the arguments, so
	   they are antecedents of the goal's rather than constraints of their own:
	   a declare-var is universally quantified, so asserting "a > 0" on its own
	   claims that every integer is positive and leaves the problem infeasible *)
	let formatted_function = format_function_to_sygus ps_sygus goal_sygus in
	let render_var x = if x = goal_sygus.x then formatted_function else x in
	let goal_sygus_constr =
		match goal_sygus.constr with
		| None -> ""
		| Some tR ->
			let obligation =
				List.fold_right
					(fun tR1 acc -> Printf.sprintf "(=> %s %s)" (tyR_to_sexp_string tR1) acc)
					(List.filter_map (fun p -> p.constr) ps_sygus)
					(tyR_to_sexp_string ~render_var tR) in
			Printf.sprintf "(constraint %s)\n" obligation in
	let sygus_input = sygus_input ^ goal_sygus_constr ^ sygus_code4 in
	sygus_input

let build_sat_input ps tFocus goal =
	let ps_parsed = List.filter_map ( fun (x, t) -> parse_tyF x t ) ps in
	let goal_parsed = Option.get (parse_tyF "" goal) in
	let consts = goal_parsed :: ps_parsed in
	let tR_focus = get_TRefinement_tyR tFocus in
	let tR_goal = get_TRefinement_tyR goal in
	let predicate = RTBOp(And, tR_focus, RTUOp(Not, tR_goal)) in
	let sat_input = "(set-logic QF_LIA)\n" in
	let rec append_declare_consts consts =
		match consts with
		| [] -> ""
		| p::consts' -> Printf.sprintf "(declare-const %s %s)\n" p.x p.tA ^ append_declare_consts consts' in
	let sat_input = sat_input ^ append_declare_consts consts in
	let sat_input = sat_input ^ "(assert " ^ tyR_to_sexp_string predicate ^ ")" in

	let sat_input = sat_input ^ "(check-sat)" in
	sat_input

(* the solver is called once per refinement goal reached, so its input and reply
   are traced under the same flag as the rules themselves rather than always *)
let debug printDebug s =
	if printDebug then print_endline s

let solve printDebug ps goal =
	let sygus_input = build_sygus_input ps goal in
	debug printDebug sygus_input;
  	let sygus_output = call_sygus sygus_input in
  	debug printDebug sygus_output;
	List.hd (parse_sygus_output sygus_output)

(* used only for refinement subtyping: inverts R1 => R2 into R1 ∧ ¬R2, if unsatisfiable, then the original predicate holds *)
let sat printDebug ps tFocus goal =
	let sat_input = build_sat_input ps tFocus goal in
	debug printDebug sat_input;
	let sat_output = call_sat sat_input in
	match sat_output with
	| "unsat" -> true 
	| _ -> false
