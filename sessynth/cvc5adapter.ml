open Sexplib;;
open Sexplib.Sexp;;
open Language;;

exception CVC5Error of string 
exception CVC5Infeasible of string 
exception CVC5ParseError of string
let sygus_code1 = {|
(set-logic NIA)
(synth-fun |}

let sygus_grammar goal_tA ints bools =
	let vars l = String.concat "" (List.map (fun x -> x ^ " ") l) in
	let start_int = Printf.sprintf {|(StartInt Int (0 1 %s
               (+ StartInt StartInt)
               (- StartInt StartInt)
               (* StartInt StartInt)))|} (vars ints) in
	let start_bool literals = Printf.sprintf {|(StartBool Bool (%s%s(and StartBool StartBool)
                    (or StartBool StartBool)
                    (not StartBool)
                    (< StartInt StartInt)
                    (<= StartInt StartInt)
                    (= StartInt StartInt)))|} literals (vars bools) in
	match goal_tA with
	| "Bool" -> Printf.sprintf {|
  ((StartBool Bool) (StartInt Int))
  (%s
   %s))
|} (start_bool "true false ") start_int
	| _ -> Printf.sprintf {|
  ((StartIte Int) (StartInt Int) (StartBool Bool))
  ((StartIte Int (StartInt
               (ite StartBool StartInt StartInt)))
   %s
   %s))
|} start_int (start_bool "")

let sygus_code4 = {|
(check-synth)
|}

let solver_timeout = 10.0

(** Runs cvc5 on [code] in the input language [lang] and returns its standard
    output, or [""] when it has not answered within [solver_timeout] -- which
    every caller reads as a failed query. A missing cvc5 is not a failed query
    but a missing dependency, and stops synthesis with a message naming it. *)
let run_cvc5 lang code =
	incr Flags.solver_calls;
	(* exec, so that the pid is cvc5's own rather than a shell's, and a timeout kills the solver itself *)
	let command = "exec cvc5 --lang=" ^ lang in
	let chans = Unix.open_process_full command (Unix.environment ()) in
	let (in_ch, out_ch, _) = chans in
	(* a cvc5 that has already exited must fail the write, not kill us *)
	let old_sigpipe = Sys.signal Sys.sigpipe Sys.Signal_ignore in
	(try output_string out_ch code; close_out out_ch with Sys_error _ -> ());
	Sys.set_signal Sys.sigpipe old_sigpipe;
	let fd = Unix.descr_of_in_channel in_ch in
	let buf = Buffer.create 1024 in
	let chunk = Bytes.create 4096 in
	let deadline = Unix.gettimeofday () +. solver_timeout in
	let rec read_all () =
		let left = deadline -. Unix.gettimeofday () in
		if left <= 0. then false
		else match Unix.select [fd] [] [] left with
			| [], _, _ -> false
			| _ ->
				let n = Unix.read fd chunk 0 (Bytes.length chunk) in
				if n = 0 then true
				else begin Buffer.add_subbytes buf chunk 0 n; read_all () end
			| exception Unix.Unix_error (Unix.EINTR, _, _) -> read_all ()
	in
	let answered = read_all () in
	if not answered then
		(try Unix.kill (Unix.process_full_pid chans) Sys.sigkill with Unix.Unix_error _ -> ());
	match Unix.close_process_full chans with
	| Unix.WEXITED 127 ->
		raise (TyUtils.Fail "cvc5 was not found on PATH, and refinement types need it")
	| _ -> if answered then Buffer.contents buf else ""

let call_sygus sygus_code = run_cvc5 "sygus2" sygus_code

(* only the first line matters: sat, unsat, unknown, or an error *)
let call_sat sat_code =
	match String.split_on_char '\n' (run_cvc5 "smt2" sat_code) with
	| line::_ -> String.trim line
	| [] -> ""

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
	| List [Atom "not"; a] -> UOp(Not, parse_sexp a)
	| List [Atom "-"; a] -> UOp(Neg, parse_sexp a)
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
	| TInt -> Some "Int"
	| TBool -> Some "Bool"
	| TUnit | TPolyVar _ | TRigidVar _ -> None

let parse_tyF x t =
	match t with
	| TAtomic(tA) -> Option.map (fun s -> { x = x; tA = s; constr = None }) (tyA_to_sygus tA)
	| TRefinement(x, tA, tR) ->
		let constr = match tR with RTBool _ -> None | _ -> Some tR in
		Option.map (fun s -> { x = x; tA = s; constr = constr }) (tyA_to_sygus tA)
	| _ -> None

(* the first binding of each name: a name bound twice would be declared twice,
   which the solver rejects *)
let dedup_by_name ps =
	List.rev (List.fold_left (fun acc p -> if List.exists (fun q -> q.x = p.x) acc then acc else p::acc) [] ps)

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
	let ps_sygus = dedup_by_name (List.filter_map ( fun (x, t) -> parse_tyF x t ) ps) in
	let goal_sygus =
		match parse_tyF "" goal with
		| Some g -> g
		| None -> raise (CVC5Error "the refinement's base type is not one the solver knows")
	in
	let sygus_input = sygus_code1 ^ goal_sygus.x ^ " (" in
	let rec append_args ps_sygus' =
		match ps_sygus' with
		| [] -> ""
		| [p] -> Printf.sprintf "(%s %s))" p.x p.tA
		| p::ps_sygus'' -> Printf.sprintf "(%s %s) " p.x p.tA ^ append_args ps_sygus'' in
	let sygus_input = sygus_input ^ append_args ps_sygus in
	let sygus_input = sygus_input ^ " " ^ goal_sygus.tA in
	let names_of tA = List.filter_map (fun p -> if p.tA = tA then Some p.x else None) ps_sygus in
	let sygus_input = sygus_input ^ sygus_grammar goal_sygus.tA (names_of "Int") (names_of "Bool") in
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

let build_sat_input ps focus_var tFocus goal =
	let ps_parsed = dedup_by_name (List.filter_map ( fun (x, t) -> parse_tyF x t ) ps) in
	match parse_tyF "" tFocus, parse_tyF "" goal with
	| None, _ | _, None -> None
	| Some focus_parsed, Some goal_parsed ->
	let v =
		match focus_var with
		| Some _ when List.exists (fun p -> p.x = focus_parsed.x) ps_parsed -> focus_parsed.x
		| _ -> "_v"
	in
	let consts =
		if List.exists (fun p -> p.x = v) ps_parsed then ps_parsed
		else { focus_parsed with x = v; constr = None } :: ps_parsed in
	let renaming binder = fun y -> if y = binder then v else y in
	let tR_focus = tyR_to_sexp_string ~render_var:(renaming focus_parsed.x) (get_TRefinement_tyR tFocus) in
	let tR_goal = tyR_to_sexp_string ~render_var:(renaming goal_parsed.x) (get_TRefinement_tyR goal) in
	let sat_input = "(set-logic QF_LIA)\n" in
	let rec append_declare_consts consts =
		match consts with
		| [] -> ""
		| p::consts' -> Printf.sprintf "(declare-const %s %s)\n" p.x p.tA ^ append_declare_consts consts' in
	let sat_input = sat_input ^ append_declare_consts consts in
	let sat_input = sat_input ^ Printf.sprintf "(assert (and %s (not %s)))" tR_focus tR_goal in
	let sat_input = sat_input ^ "(check-sat)" in
	Some sat_input

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

(* used only for refinement subtyping: inverts R1 => R2 into R1 ∧ ¬R2, if unsatisfiable, then the original predicate holds.
   [focus_var] names the variable in focus, when the focus is one rather than an application *)
let sat ?focus_var printDebug ps tFocus goal =
	match build_sat_input ps focus_var tFocus goal with
	| None -> false
	| Some sat_input ->
		debug printDebug sat_input;
		let sat_output = call_sat sat_input in
		debug printDebug sat_output;
		match sat_output with
		| "unsat" -> true
		| _ -> false
