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
(* Values and variables *)
|	Int of int
|	Bool of bool
|	List of exp list
|	LVar of string
|	Let of string * exp * exp (* let x = M in N *)
(* Operations *)
|	Eq of exp * exp (* M == N *)
|	LessThan of exp * exp (* M < N *)
|	LessEq of exp * exp (* M <= N *)
|	GrThan of exp * exp (* M > N *)
|	GrEq of exp * exp (* M >= N *)
|	Sum of exp * exp (* M + N *)
|	Sub of exp * exp (* M - N *)
|	Mult of exp * exp (* M * N *)
|	Div of exp * exp (* M / N *)
(* A ⊕ B *)
|	LDisjInL of exp * expType (* M:A then Inl_B M : A ⊕ B *)
|	LDisjInR of expType * exp (* M:B then Inr_A M : A ⊕ B *)
|	LDisjCase of exp * string * exp * string * exp (* case M of inl x -> e1, inr y -> e2 *)
(* A ⊗ B *)
|	LConjPair of exp * exp (* Multiplicative pair: (M1 * M2) : A ⊗ B *)
|	Let2 of string * string * exp * exp (* let x * y = M in N *)
(* A & B *)
|	LAltConjPair of exp * exp (* Additive pair: (M1 * M2) : A & B *)
|	LAltConjFst of exp (* fst (M1,M2) -> M1 *)
|	LAltConjSnd of exp (* snd (M1,M2) -> M2 *)
(* (A1, ..., An) ⊸ B *)
|	LFunc of (string * expType) list * exp (* fun [(paramName1, paramType1); ...; (paramNameN, paramTypeN)] -o eBody *)
|	LFuncCall of exp * exp list (* (eLFunc) eArg1 ... eArgN *)

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
	|	Eq _ -> "Eq"
	|	LessThan _ -> "LessThan"
	|	LessEq _ -> "LessEq"
	|	GrThan _ -> "GrThan"
	|	GrEq _ -> "GrEq"
	|	Sum _ -> "Sum"
	|	Sub _ -> "Sub"
	|	Mult _ -> "Mult"
	|	Div _ -> "Div"
	|	LDisjInL _ -> "LDisjInL"
	|	LDisjInR _ -> "LDisjInR"
	|	LDisjCase _ -> "LDisjCase"
	|	LConjPair _ -> "LConj"
	|	Let2 _ -> "LLet"
	|	LAltConjPair _ -> "LAltConj"
	|	LAltConjFst _ -> "LAltConjFst"
	|	LAltConjSnd _ -> "LAltConjSnd"
	|	LFunc _ -> "Func"
	|	LFuncCall _ -> "FuncCall"

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
	|	Eq(e1, e2) -> typeofEq env e1 e2
	|	LessThan(e1, e2)
	|	LessEq(e1, e2)
	|	GrThan(e1, e2)
	|	GrEq(e1, e2) -> typeofIntCompare env e1 e2
	|	Sum(e1, e2)
	|	Sub(e1, e2)
	|	Mult(e1, e2)
	|	Div(e1, e2) -> typeofIntOp env e1 e2
	|	LDisjInL(e, t) -> let env', t1 = typeof env e in env', TLDisjPair(t1, t)
	|	LDisjInR(t, e) -> let env', t2 = typeof env e in env', TLDisjPair(t, t2) 
	|	LDisjCase(e, x, el, y, er) -> typeofLDisjCase env e x el y er
	|	LConjPair(e1, e2) -> typeofLConjPair env e1 e2
	|	Let2(x, y, e1, e2) -> typeofLet2 env x y e1 e2
	|	LAltConjPair(e1, e2) -> typeofLAltConjPair env e1 e2
	|	LAltConjFst(e) -> typeofLAltConjElem env e true
	|	LAltConjSnd(e) -> typeofLAltConjElem env e false
	|	LFunc(pNameTypeTupleList, b) -> typeofFunc env pNameTypeTupleList b
	|	LFuncCall(e1, e2) -> typeofFuncCall env e1 e2

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

and typeofEq env e1 e2 =
	let env', e1Type = typeof env e1 in
	let env'', e2Type = typeof env' e2 in
	env'', TBool

and typeofIntCompare env e1 e2 =
	let env', e1Type = typeof env e1 in
	let env'', e2Type = typeof env' e2 in
	match e1Type, e2Type with
	|	TInt, TInt -> env'', TBool
	|	_ -> raise (TypeError("Integer comparison requires a pair of type (TInt, TInt), provided: (" ^ expTypeToString e1Type ^ ", " ^ expTypeToString e2Type ^ ")"))

and typeofIntOp env e1 e2 =
	let env', e1Type = typeof env e1 in
	let env'', e2Type = typeof env' e2 in
	match e1Type, e2Type with
	|	TInt, TInt -> env'', TInt
	|	_ -> raise (TypeError("Integer operation requires a pair of type (TInt, TInt), provided: (" ^ expTypeToString e1Type ^ ", " ^ expTypeToString e2Type ^ ")"))


and typeofLDisjCase env e x el y er =
	let env', eType = typeof env e in 
		match eType with
		|	TLDisjPair(t1, t2) ->
				let envt1 = envBind env' [(x, t1)] in
				let envl, elType = typeof envt1 el in
				let envt2 = envBind env' [(y, t2)] in
				let envr, erType = typeof envt2 er in
				if (=) elType erType then
					if List.equal (=) envl envr then envl, elType
					else begin
						print_string "Left: ["; printEnv envl; print_endline "]";
						print_string "Right: ["; printEnv envr; print_endline "]";
						raise (TypeError("LDisjCase left and right expressions do not consume the same linear variables"))
					end
				else raise (TypeError(expTypeToString eType ^ " has incompatible branch return types: (" ^ expTypeToString elType ^ ", " ^ expTypeToString erType ^ ")"))
		|	_ -> raise (TypeError("TLDisjPair type expected"))

and typeofLConjPair env e1 e2 =
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

and typeofLAltConjPair env e1 e2 =
	let env1, t1 = typeof env e1 in
	let env2, t2 = typeof env e2 in
	if List.equal (=) env1 env2 then env1, TLAltConjPair(t1, t2)
	else begin
		print_string "Left: ["; printEnv env1; print_endline "]";
		print_string "Right: ["; printEnv env2; print_endline "]";
		raise (TypeError("LAltConjPair left and right expressions do not consume the same linear variables"))
	end

and typeofLAltConjElem env e isFst =
	let env', t = typeof env e in
	match t with
	|	TLAltConjPair(e1, e2) -> 
			if isFst then env', e1
			else env', e2
	|	_ -> raise (TypeError("TLAltConjPair type expected"))

and typeofFunc env pNameTypeTupleList b =
	let env' = envBind env pNameTypeTupleList in
	let _, pTypes = List.split pNameTypeTupleList in
	let env'', bType = typeof env' b in
	env'', TLFunc(pTypes, bType)

and typeofFuncCall env e1 e2 =
	let env', e1Type = typeof env e1 in
	let env'', e2Type = formatTypeofExpList (typeofExpList env' e2) in
	match e1Type, e2Type with
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
case z of inl a -> a + y, inr b -> b + 1
*)

let exp = 
	Let("x", Int(2), 
	Let("y", Int(4),
	Let("z", LDisjInR(TInt, LVar("x")),
	LDisjCase(LVar("z"), "a", Sum(LVar("a"), LVar("y")), "b", Sum(LVar("b"), LVar("y")))
	)))
;;

(*
exp in concrete syntax:

let x = 2 in
let y = 4 in
let z = x & x in
y + snd z
*)

let exp = 
	Let("x", Int(2), 
	Let("y", Int(4),
	Let("z", LAltConjPair(LVar("x"), LVar("x")),
	Sum(LVar("y"), LAltConjSnd(LVar("z")))
	)))
;;

(*
let x = 2 in
let y = 4 in
fun x y -o x + y	(TInt -o TInt -o TInt)
*)
let exp = 
	Let("x", Int(2), 
	Let("y", Int(4),
	LFuncCall(LFunc([("a", TInt); ("b", TInt)], Sum(LVar("a"), LVar("b"))), [LVar("x"); LVar("y")])
	))
;;

let env, expType = typeof env exp;;

print_endline ("Expression type: " ^ (expTypeToString expType));;
if not (List.is_empty env) then begin
	print_string "Unused resources: "; 
	printEnv env;
	print_endline ""
end;;
print_endline "Type checking complete";;
