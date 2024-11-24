exception TypeError of string
exception BindingError of string

(* Type definitions *)

type expType =
|	TInt
|	TBool
|	TTList of expType list
|	TLDisjPair of expType * expType
|	TLConjPair of expType * expType
|	TLAltConjPair of expType * expType
|	TLFunc of expType list * expType

type exp =
|	Int of int
|	Bool of bool
|	List of exp list
|	LVar of string
|	Let of string * exp * exp (* let x = exp1 in exp2 *)
(* A ⊕ B *)
|	LDisjInL of exp * expType (* M:A then Inl_B M : A ⊕ B *)
|	LDisjInR of expType * exp (* M:B then Inr_A M : A ⊕ B *)
|	LDisjCase of exp * string * exp * string * exp (* case M of inl x -> E1, inr y -> E2 *)
(* A ⊗ B *)
|	LConj of exp * exp (* Multiplicative pair: (M1 * M2) : A ⊗ B *)
|	Let2 of string * string * exp * exp (* let x * y = M in N *)
(* A & B *)
|	LAltConj of exp * exp (* Additive pair: (M1 * M2) : A & B *)
|	LAltConjFst of exp (* fst (M1,M2) -> M1 *)
|	LAltConjSnd of exp (* snd (M1,M2) -> M2 *)
(* (A1, ..., An) ⊸ B *)
|	Func of (string * expType) list * exp (* fun [(pName1, pType1); ...; (pNameN, pTypeN)] -> expBody *)
|	FuncCall of exp * exp list (* (expFunc) expArg *)

(* Auxiliary *)

let rec removeLast l = 
	match l with 
	|	[] -> [] 
	|	[_] -> [] 
	|	hd::tl -> hd::(removeLast tl);;

let formatTypeofExpList l = 
	let envs, expTypes = List.split l in
	List.nth envs ((List.length envs)-1), TTList(expTypes)

(* Printing stuff *)

let expToString exp =
	match exp with
	|	Int _ -> "Int"
	|	Bool _ -> "Bool"
	|	List _ -> "List"
	|	LVar _ -> "LVar"
	|	Let _ -> "Let"
	|	LDisjInL _ -> "LDisjInL"
	|	LDisjInR _ -> "LDisjInR"
	|	LDisjCase _ -> "LDisjCase"
	|	LConj _ -> "LConj"
	|	Let2 _ -> "LLet"
	|	LAltConj _ -> "LAltConj"
	|	LAltConjFst _ -> "LAltConjFst"
	|	LAltConjSnd _ -> "LAltConjSnd"
	|	Func _ -> "Func"
	|	FuncCall _ -> "FuncCall"

let rec expTypeToString expType =
	match expType with
	|	TInt -> "TInt"
	|	TBool -> "TBool"
	|	TTList(termTypeList) -> "TTList(" ^ (expTypeListToString termTypeList) ^ ")"
	|	TLAltConjPair(t1, t2) -> "TLAltConjPair(" ^ (expTypeToString t1) ^ ", " ^ (expTypeToString t2) ^ ")"
	|	TLConjPair(t1, t2) -> "TLConjPair(" ^ (expTypeToString t1) ^ ", " ^ (expTypeToString t2) ^ ")"
	|	TLDisjPair(t1, t2) -> "TLDisjPair(" ^ (expTypeToString t1) ^ ", " ^ (expTypeToString t2) ^ ")"
	|	TLFunc(t1, t2) -> "TLFunc(" ^ (expTypeListToString t1) ^ (expTypeToString t2) ^ ")"	

and expTypeListToString expTypeList =
	match expTypeList with
	|	[] -> ""
	|	t::termTypeList' -> (expTypeToString t) ^ ", " ^ (expTypeListToString termTypeList');;

let rec printEnv env = 
	match env with
	|	[] -> ()
	|	(n, t)::env' -> print_string (n ^ ":" ^ (expTypeToString t)); print_string ", "; printEnv env';;

(* Type checker *)

let env : (string * expType) list = []

let envLookup env n =
	try
		let nType = List.assoc n env in
		let env' = List.remove_assoc n env in
		env', nType
	with Not_found -> raise (BindingError("Linear variable " ^ n ^ " has not been defined, or has already been consumed"))

let envBind env nameTypeTupleList =
	nameTypeTupleList @ env

let rec typeof env exp =
    match exp with
	|	Int(i) -> env, TInt
	|	Bool(b) -> env, TBool
	|	List(expList) -> formatTypeofExpList (typeofExpList env expList)
	|	LVar(n) -> envLookup env n
	|	Let(n, e1, e2) -> typeofLet env n e1 e2
	|	LDisjInL(e, t) -> let env', t1 = typeof env e in env', TLDisjPair(t1, t)
	|	LDisjInR(t, e) -> let env', t2 = typeof env e in env', TLDisjPair(t, t2) 
	|	LDisjCase(e, x, el, y, er) -> typeofLDisjCase env e x el y er
	|	LConj(e1, e2) -> typeofLConj env e1 e2
	|	Let2(x, y, e1, e2) -> typeofLet2 env x y e1 e2
	|	LAltConj(e1, e2) -> typeofLAltConj env e1 e2
	|	LAltConjFst(e) -> env, TInt
	|	LAltConjSnd(e) -> env, TInt
	|	Func(pNameTypeTupleList, b) -> typeofFunc env pNameTypeTupleList b
	|	FuncCall(e1, e2) -> typeofFuncCall env e1 e2

and typeofExpList env expList =
	match expList with
	|	[] -> []
	|	exp::expList' -> 
			let env', expType = typeof env exp in
			(env', expType)::typeofExpList env' expList'

and typeofLet env n e1 e2 =
	let env', e1Type = typeof env e1 in
	let env'' = envBind env' [(n, e1Type)] in
	typeof env'' e2

and typeofLDisjCase env e x el y er =
	let env', eType = typeof env e in 
		match eType with
		|	TLDisjPair(t1, t2) ->
				let envt1 = envBind env' [(x, t1)] in
				let envl, elType = typeof envt1 el in
				let envt2 = envBind env' [(y, t2)] in
				let envr, erType = typeof envt2 er in
				if (=) elType erType then env', elType
				else raise (TypeError(expTypeToString eType ^ " has incompatible branch return types (" ^ expTypeToString elType ^ ", " ^ expTypeToString erType ^ ")"))
		|	_ -> raise (TypeError("TLDisjPair type expected"))

and typeofLConj env e1 e2 =
	let env', t1 = typeof env e1 in 
	let env'', t2 = typeof env' e2 in
    env'', TLConjPair(t1, t2)

and typeofLet2 env x y e1 e2 =
	let env', e1Type = typeof env e1 in
	match e1Type with
	|	TLConjPair(t1, t2) -> 
			let env'' = envBind env' [(x, t1); (y, t2)] in
			typeof env'' e2
	|	_ -> raise (TypeError("TLConjPair type expected"))

and typeofLAltConj env e1 e2 =
	let env1, t1 = typeof env e1 in
	let env2, t2 = typeof env e2 in
	(* Verificar algo sobre env1 e env2 *)
	env2, TLAltConjPair(t1,t2)

and typeofFunc env pNameTypeTupleList b =
	let env' = envBind env pNameTypeTupleList in
	let _, pTypes = List.split pNameTypeTupleList in
	let env'', bType = typeof env' b in
	env'', TLFunc(pTypes, bType)

and typeofFuncCall env e1 e2 =
	let env', expType = typeof env e1 in
	let env'', argTypes = formatTypeofExpList (typeofExpList env' e2) in
	match expType, argTypes with
	|	TLFunc(pTypes, bType), TTList(argTypes) ->
			if (=) pTypes argTypes then env'', bType
			else raise (TypeError("Type mismatch on FuncCall parameter, expected: " ^ expTypeToString (TTList(pTypes)) ^ ", provided: " ^ expTypeToString (TTList(argTypes))))
	|	_ -> raise (TypeError("TLFunc type expected"))

(* Running stuff *)

(*
exp in concrete syntax:

let x = 2 in
let y = 4 in
let z = inl x in
case z of inl a -> a, inr b -> b
*)

let exp = 
	Let("x", Int(2), 
	Let("y", Int(4),
	Let("z", LDisjInR(TBool, LVar("x")),
	LDisjCase(LVar("z"), "a", LVar("a"), "b", LVar("b"))
	)))
;;

let env, expType = typeof env exp;;

print_endline ("Expression type: " ^ (expTypeToString expType));;
if not (List.is_empty env) then begin
	print_string "Unused resources: "; 
	printEnv env;
	print_endline ""
end;;
print_endline "Type checking complete";;
