open Language

let uOp_to_string t =
    match t with
    | Not -> "!"
    | Neg -> "-"

let bOp_to_string t =
    match t with
    | And -> " && "
    | Or -> " || "
    | Eq -> " == "
    | Gr -> " > "
    | Lt -> " < "
    | GrE -> " >= "
    | LtE -> " <= "
    | Sum -> " + "
    | Sub -> " - "
    | Mult -> " * "
    | Div -> " / "

let tyA_to_string t =
    match t with
    | TInt -> "int"
    | TBool -> "bool"
    | TPolyVar a -> a

let tyK_to_string t =
    match t with
    | KBase -> "*"
    | KArrow -> "* -> *"

let rec tyR_to_string t =
    match t with
    | RTUOp(op, t) -> uOp_to_string op ^ tyR_to_string t
    | RTBOp(op, t1, t2) -> tyR_to_string t1 ^ bOp_to_string op ^ tyR_to_string t2
    | RTInt(v) -> string_of_int v
    | RTBool(v) -> string_of_bool v
    | RTVar(x) -> x

and tyF_to_string t =
    match t with 
    | TAtomic(t) -> tyA_to_string t
    | TRefinement(x, t1, RTBool(true)) -> x ^ ":" ^ tyA_to_string t1
    | TRefinement(x, t1, t2) -> "{" ^ x ^ ":" ^ tyA_to_string t1 ^ " | " ^ tyR_to_string t2 ^ "}"
    | TArrow(t1, t2) -> tyF_to_string t1 ^ " -> " ^ tyF_to_string t2
    | TProcess(incsl, outs) -> "{" ^ cs_list_to_string incsl ^ " |- " ^ tyS_to_string outs ^ "}"
    | TDeclr(x) -> x
    | TForAll(xkl, t) -> "∀" ^ id_kind_list_to_string xkl ^ ". " ^ tyF_to_string t
    | TConstructor(x, args) -> x ^ " " ^ tyF_args_to_string args

and tyF_args_to_string args =
    match args with
    | [] -> ""
    | [t] -> tyF_to_string t
    | t::args' -> tyF_to_string t ^ " " ^ tyF_args_to_string args'

and tyS_to_string t =
    match t with 
    | STDeclr(x) -> x
    | STSendF(t1, t2) -> tyF_to_string t1 ^ " ∧ " ^ tyS_to_string t2
    | STRecvF(t1, t2) -> tyF_to_string t1 ^ " ⊃ " ^ tyS_to_string t2
    | STSendS(t1, t2) -> tyS_to_string t1 ^ " ⊗ " ^ tyS_to_string t2
    | STRecvS(t1, t2) -> tyS_to_string t1 ^ " -o " ^ tyS_to_string t2
    | STExtChoice(xtl) -> "&{" ^ label_tyS_list_to_string xtl ^ "}"
    | STIntChoice(xtl) -> "⊕{" ^ label_tyS_list_to_string xtl ^ "}"
    | STRec(x, t) -> "𝜇" ^ x ^ "." ^ tyS_to_string t
    | STRecVar(x) -> x
    | STUnit -> "1"

and cs_list_to_string csl =
    match csl with
    | [] -> ""
    | [(c, s)] -> c ^ ":" ^ tyS_to_string s
    | (c, s)::csl' -> c ^ ":" ^ tyS_to_string s ^ ", " ^ cs_list_to_string csl'

and label_tyS_list_to_string xtl =
    match xtl with
    | [] -> ""
    | [(l, t)] -> l ^ ":" ^ tyS_to_string t
    | (l, t)::xtl' -> l ^ ":" ^ tyS_to_string t ^ ", " ^ label_tyS_list_to_string xtl' 
	
and id_kind_list_to_string xkl =
    match xkl with
    | [] -> ""
    | [(x, k)] -> x ^ ":" ^ tyK_to_string k
    | (x, k)::xkl' -> x ^ ":" ^ tyK_to_string k ^ ", " ^ id_kind_list_to_string xkl'

let rec expF_to_string e depth =
    match e with 
    | Int(v) -> string_of_int v
    | Bool(v) -> string_of_bool v
    | UOp(op, e) -> uOp_to_string op ^ expF_to_string e depth
    | BOp(op, e1, e2) -> expF_to_string e1 depth ^ bOp_to_string op ^ expF_to_string e2 depth
    | Var x -> x
    | Let(x, e1, e2) -> "let" ^ x ^ " = " ^ expF_to_string e1 depth ^ " in " ^expF_to_string e2 depth
    | Lam(x, t, e) -> x ^ " -> " ^ expF_to_string e depth
    | App(e1, e2) -> "(" ^ expF_to_string e1 depth ^ ") " ^ expF_to_string e2 depth
    | Ite(e1, e2, e3) -> "if " ^ expF_to_string e1 depth ^ " then " ^ expF_to_string e2 depth ^ " else " ^ expF_to_string e3 depth
    | Process(c, eP, tS, xtl) -> c ^ " <- {\n" ^ expP_to_string eP (depth + 1) ^ "}" ^ process_input_channels_to_string xtl ^ "\n"
    | LetRec(x, t, eF) -> "let rec " ^ x ^ " = " ^ expF_to_string eF depth
    | Constructor(x, args) -> x ^ " " ^ expF_args_to_string args
    | Match(e1, cons_exp_list) -> "match " ^ expF_to_string e1 depth ^ " with " ^ match_cases_to_string cons_exp_list depth

and process_input_channels_to_string xtl =
    if List.is_empty xtl then ""
    else " <- [" ^ label_tyS_list_to_string xtl ^ "]" 

and expF_args_to_string args =
    match args with
    | [] -> ""
    | [t] -> expF_to_string t 0
    | t::args' -> expF_to_string t 0 ^ " " ^ expF_args_to_string args'

and match_cases_to_string cons_exp_list depth =
    match cons_exp_list with
    | [] -> ""
    | [(x, args, e)] -> x ^ " " ^ id_args_to_string args ^ " -> " ^ expF_to_string e depth
    | (x, args, e)::args' -> x ^ " " ^ id_args_to_string args ^ " -> " ^ expF_to_string e depth ^ " | " ^ match_cases_to_string args' depth

and id_args_to_string args =
    match args with
    | [] -> ""
    | [x] -> x
    | x::args' -> x ^ " " ^ id_args_to_string args'

and expP_to_string e depth =
    let indent = String.make (depth * 2) ' ' in
    indent ^
    match e with 
    | SendF(c, eF, eP) -> "send " ^ c ^ " " ^ expF_to_string eF 0 ^ ";\n" ^ expP_to_string eP depth
    | RecvF(x, tF, c, eP) -> x ^ " <- recv " ^ c ^ ";\n" ^ expP_to_string eP depth
    | SendS(c1, c2, eP1, eP2) -> "send " ^ c1 ^ " (" ^ c2 ^ " <- " ^ expP_to_string eP1 0 ^ ");\n" ^ expP_to_string eP2 depth
    | RecvS(x, tS, c, eP) -> x ^ " <- recv " ^ c ^ ";\n" ^ expP_to_string eP depth
    | Close(c) -> "close " ^ c ^ "\n"
    | Wait(c, eP) -> "wait " ^ c ^ ";\n" ^ expP_to_string eP depth
    | Fwd(c1, c2, tS) -> "fwd " ^ c1 ^ " " ^ c2 ^ "\n"
    | Choice(c, labelprocesslist) -> "case " ^ c ^ " of\n" ^ label_process_list_to_string labelprocesslist depth
    | ChoiceSelect(c, l, eP) -> c ^ "." ^ l ^ ";\n" ^ expP_to_string eP depth
    | Spawn(c, eF, cl, eP) -> c ^ " <- spawn " ^ expF_to_string eF 0 ^ spawn_c_list_to_string cl ^ ";\n" ^ expP_to_string eP depth

and label_process_list_to_string labelprocesslist depth = 
    let indent = String.make (depth * 2) ' ' in
    match labelprocesslist with
    | [] -> ""
    | (l, eP)::labelprocesslist' -> indent ^ l ^ ":\n" ^ expP_to_string eP (depth + 1) ^ label_process_list_to_string labelprocesslist' depth

and spawn_c_list_to_string cl =
    if List.is_empty cl then ""
    else " [" ^ c_list_to_string cl ^ "]" 

and c_list_to_string cl =
    match cl with
    | [] -> ""
    | [c] -> c
    | c::cl' -> c ^ "; " ^ c_list_to_string cl'

let delta_to_string delta = 
  let rec delta_to_string' delta =
    match delta with
    | [] -> "";
    | [(x, t)] -> x ^ ":" ^ tyS_to_string t
    | (x, t)::c' -> x ^ ":" ^ tyS_to_string t ^ "; " ^ delta_to_string' c'
  in "[" ^ delta_to_string' delta ^ "]"  

let psi_to_string psi = 
  let rec psi_to_string' psi =
    match psi with
    | [] -> "";
    | [(x, t)] -> x ^ ":" ^ tyF_to_string t
    | (x, t)::c' -> x ^ ":" ^ tyF_to_string t ^ "; " ^ psi_to_string' c'
  in "[" ^ psi_to_string' psi^ "]"
