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
|	LDisjInR of exp * expType (* M:B then Inr_A M : A ⊕ B *)
|	LDisjCase of exp * string * exp * string * exp (* case M of inl x -> E1, inr x -> E2 *)
(* A ⊗ B *)
|	LConj of exp * exp (* Multiplicative pair: (M1 * M2) : A ⊗ B *)
|	Let2 of exp * string * string * exp (* let x * y = M in N *)
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
	|	LDisjInL(e, t) -> let env, t1 = typeof env e in env, TLDisjPair(t1, t)
	|	LDisjInR(e, t) -> let env, t2 = typeof env e in env, TLDisjPair(t, t2) 
	|	LDisjCase(e, x, el, y, er) -> typeofLDisjCase env e x el y er
	|	LConj(e1, e2) -> typeofLConj env e1 e2
	|	Let2(e1, x, y, e2) -> typeofLet2 env e1 x y e2
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
	let env, eType = typeof env e in 
		match eType with
		|	TLDisjPair(t1, t2) -> env, TInt
		|	_ -> raise (TypeError(""))
		(* tipificar el em que x:t1, tipificar er em que y:t2 e tipo de el e er iguais *)

and typeofLConj env e1 e2 =
	let env', t1 = typeof env e1 in 
	let env'', t2 = typeof env' e2 in
    env'', TLConjPair(t1,t2)

and typeofLet2 env e1 x y e2 =
	let env', e1Type = typeof env e1 in
	match e1Type with
	|	TLConjPair(t1,t2) -> env', TInt
	|	_ -> raise (TypeError(""))
	(* tipificar e2 num ambiente em que x:t1,y:t2 *)

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
	|	_ -> raise (TypeError("Arrow function type expected"))

(* Running stuff *)

(*
Concrete syntax of exp below:

let x = 2 in
let y = 4 in
let z = 6 in
(fun x:int y:int -> x ^ y) x y

Env should look like: 
	After typeofFunc line 1: [f1.x:TInt, f1.y:TInt, z:TInt, y:TInt, x:TInt]
	After typeofFunc line 3: [z:TInt, y:TInt, x:TInt] ... typeofFunc returns TTList([TInt, TInt, TInt]) aka TInt -> TInt -> TInt
	After typeofFuncCall line 2: [z:TInt]
	Unused resources: [z:TInt]
*)

let exp = 
	Let("x", Int(2), 
	Let("y", Int(4),
	Let("z", Int(6),
		FuncCall(Func([("f1.x", TInt); ("f1.y", TInt)], LConj(LVar("f1.x"), LVar("f1.y"))), [LVar("x"); LVar("y")])
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
