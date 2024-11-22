exception TypeError of string
exception BindingError of string

(* Type definitions *)

type termType =
|	TInt
|	TBool
|	TTList of termType list
|	APair of termType * termType 
|	MPair of termType * termType 
|	LSum of termType * termType
|	LFun of termType list * termType 

type term =
|	Int of int
|	Bool of bool
|	LVar of string
|	Let of string * term * term (* let x = exp1 in exp2 *)
(* A ⊕ B *)
|	LDisjInL of term * termType (* M:A then Inl_B M : A ⊕ B *)
|	LDisjInR of term * termType (* M:B then Inr_A M : A ⊕ B *)
|	LDisjCase of term * string * term * string * term (* case M of inl x -> E1, inr x -> E2 *)
(* A ⊗ B *)
|	LConj of term * term (* Multiplicative pair: (M1 * M2) : A ⊗ B *)
|	LLet of term * string * string * term (* let x * y = M in N *)
(* A & B *)
|	LAltConj of term * term (* Additive pair: (M1 * M2) : A & B *)
|	LAltConjFst of term (* fst (M1,M2) -> M1 *)
|	LAltConjSnd of term (* snd (M1,M2) -> M2 *)
(* (A1, ..., An) ⊸ B *)
|	Func of (string * termType) list * term (* fun [(paramName1, paramType1); ...; (paramNameN, paramTypeN)] -> expBody *)
|	FuncCall of term * term list (* (expFunc) expArg *)

(* Auxiliary *)

let rec removeLast l = 
	match l with 
	|	[] -> [] 
	|	[_] -> [] 
	|	hd::tl -> hd::(removeLast tl);;

let formatTypeofExpList l = 
	let envs, expTypes = List.split l in
	List.nth envs ((List.length envs)-1), expTypes

(* Printing stuff *)

let termToString term =
	match term with
	|	Int _ -> "Int"
	|	Bool _ -> "Bool"
	|	LVar _ -> "LVar"
	|	Let _ -> "Let"
	|	LDisjInL _ -> "LDisjInL"
	|	LDisjInR _ -> "LDisjInR"
	|	LDisjCase _ -> "LDisjCase"
	|	LConj _ -> "LConj"
	|	LLet _ -> "LLet"
	|	LAltConj _ -> "LAltConj"
	|	LAltConjFst _ -> "LAltConjFst"
	|	LAltConjSnd _ -> "LAltConjSnd"
	|	Func _ -> "Func"
	|	FuncCall _ -> "FuncCall"

let rec termTypeToString termType =
	match termType with
	|	TInt -> "TInt"
	|	TBool -> "TBool"
	|	TTList(termTypeList) -> "TTList(" ^ (termTypeListToString termTypeList) ^ ")"
	|	APair(t1, t2) -> "APair(" ^ (termTypeToString t1) ^ ", " ^ (termTypeToString t2) ^ ")"
	|	MPair(t1, t2) -> "MPair(" ^ (termTypeToString t1) ^ ", " ^ (termTypeToString t2) ^ ")"
	|	LSum(t1, t2) -> "LSum(" ^ (termTypeToString t1) ^ ", " ^ (termTypeToString t2) ^ ")"
	|	LFun(t1, t2) -> "LFun(" ^ (termTypeListToString t1) ^ (termTypeToString t2) ^ ")"	

and termTypeListToString termTypeList =
	match termTypeList with
	|	[] -> ""
	|	t::termTypeList' -> (termTypeToString t) ^ ", " ^ (termTypeListToString termTypeList');;

let rec printEnv env = 
	match env with
	|	[] -> ()
	|	(n, t)::env' -> print_string (n ^ ":" ^ (termTypeToString t)); print_string ", "; printEnv env';;

(* Type checker *)

let env : (string * termType) list = []

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
	|	LVar(n) -> envLookup env n
	|	Let(n, e1, e2) -> typeofLet env n e1 e2
	|	LDisjInL(e, t) -> let env, t1 = typeof env e in env, LSum(t1, t)
	|	LDisjInR(e, t) -> let env, t2 = typeof env e in env, LSum(t, t2) 
	|	LDisjCase(e, x, el, y, er) -> typeofLDisjCase env e x el y er
	|	LConj(e1, e2) -> typeofLConj env e1 e2
	|	LLet(e1, x, y, e2) -> typeofLLet env e1 x y e2
	|	LAltConj(e1, e2) -> typeofLAltConj env e1 e2
	|	LAltConjFst(e) -> env, TInt
	|	LAltConjSnd(e) -> env, TInt
	|	Func(pNameTypeTupleList, b) -> typeofFunc env pNameTypeTupleList b
	|	FuncCall(t1, t2) -> typeofFuncCall env t1 t2

and typeofExpList env expList =
	match expList with
	|	[] -> []
	|	exp::expList' -> 
			let env', expType = typeof env exp in
			(env', expType)::typeofExpList env' expList'

and typeofLet env n e1 e2 =
	let env', t1Type = typeof env e1 in
	let env'' = envBind env' [(n, t1Type)] in
	typeof env'' e2

and typeofLDisjCase env e x el y er =
	let env, eType = typeof env e in 
		match eType with
		|	LSum(t1, t2) -> env, TInt
		|	_ -> raise (TypeError(""))
		(* tipificar el em que x:t1, tipificar er em que y:t2 e tipo de el e er iguais *)

and typeofLConj env e1 e2 =
	let env' , t1 = typeof env e1 in 
	let env'' , t2 = typeof env' e2 in
    env'', MPair(t1,t2)

and typeofLLet env e1 x y e2 =
	let env' , e1Type = typeof env e1 in
	match e1Type with
	|	MPair(t1,t2) -> env', TInt
	|	_ -> raise (TypeError(""))
	(* tipificar e2 num ambiente em que x:t1,y:t2 *)

and typeofLAltConj env e1 e2 =
	let env1, t1 = typeof env e1 in 
	let env2, t2 = typeof env e2 in 
	(* Verificar algo sobre env1 e env2 *)
	env2, APair(t1,t2)

and typeofLPair env op t1 t2 =
	match t1, t2 with
	|	LVar(n1), LVar(n2) ->
			let env', t1t = typeof env t1 in 
			let env'', t2t = typeof env' t2 in
			if (=) t1t t2t then env'', t1t
			else raise (TypeError("Type mismatch on " ^ (termToString op) ^ " operation, provided: " ^ (termTypeToString t1t) ^ " " ^ (termTypeToString t2t)))
	|	_ -> raise (TypeError("Must pass LVar parameters on " ^ (termToString op) ^ " operation, provided: " ^ (termToString t1) ^ " " ^ (termToString t2)))

and typeofFunc env pNameTypeTupleList b =
	let env' = envBind env pNameTypeTupleList in
	let _, pTypes = List.split pNameTypeTupleList in
	let env'', bType = typeof env' b in
	env'', TTList(pTypes @ [bType])

and typeofFuncCall env f args =
	let env', fType = typeof env f in
	let env'', argTypes = formatTypeofExpList (typeofExpList env' args) in
	match fType, argTypes with
	|	TTList(fTypes), argTypes ->
			let pTypes = removeLast fTypes in
			let bType = List.nth fTypes ((List.length fTypes)-1) in
			if (=) pTypes argTypes then env'', bType
			else raise (TypeError("Type mismatch on FuncCall parameter, expected: " ^ termTypeToString (TTList(pTypes)) ^ ", provided: " ^ termTypeToString (TTList(argTypes))))
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

print_endline ("Expression type: " ^ (termTypeToString expType));;
if not (List.is_empty env) then begin
	print_string "Unused resources: "; 
	printEnv env;
	print_endline ""
end;;
print_endline "Type checking complete";;
