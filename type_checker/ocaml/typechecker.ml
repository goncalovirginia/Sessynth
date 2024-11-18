
exception TypeError of string

type termType =
|   TInt
|	TBool

type term =
|   Int of int
|	Bool of bool
|	Var of string
|	Let of string * termType * term * term (* let x : t = exp1 in exp2 *)
|	Sum of term * term
|	Mult of term * term
|	Eq of term * term
|	LEq of term * term
|	GEq of term * term
|	And of term * term
|	Or of term * term
|   Func of string * termType * term (* fName param1Type expBody *)
|   FuncCall of string * term (* fname expParam *)

let termToString term =
	match term with
	|	Int _ -> "Int"
	|	Bool _ -> "Bool"
	|	Var _ -> "Var"
	|	Let _ -> "Let"
	|	Sum _ -> "Sum"
	|	Mult _ -> "Mult"
	|	Eq _ -> "Eq"
	|	LEq _ -> "LEq"
	|	GEq _ -> "GEq"
	|	And _ -> "And"
	|	Or _ -> "Or"
	|	Func _ -> "Func"
	|	FuncCall _ -> "FuncCall"

let termTypeToString termType =
	match termType with
	|	TInt -> "TInt"
	|	TBool -> "TBool"

let env = []

let envLookup env n =
	List.assoc n env

let envAssign env n nType =
	(n, nType) :: env

let rec typeof env exp =
    match exp with
	|	Int(i) -> TInt
	|	Bool(b) -> TBool
	|	Var(n) -> envLookup env n
	|	Let(n, nType, t1, t2) -> typeOfLet env n nType t1 t2
	|	Sum(t1, t2)
	|	Mult(t1, t2)
	|	Eq(t1, t2)
	|	LEq(t1, t2)
	|	GEq(t1, t2)
	|	And(t1, t2)
	|	Or(t1, t2) -> typeofPairOp env exp t1 t2
	|	Func(n, paramType, b) -> typeof env b
	|	FuncCall(n, t1) -> envLookup env n (*let funcTypes = envLookup env f in let funcArgType = typeof env t1 in
			List.nth funcTypes 0 equals funcArgType then funcTypes[0]*)

and typeofPairOp env pairOp t1 t2 =
	match pairOp, typeof env t1, typeof env t2 with
	|	Sum _, TInt, TInt -> TInt
	|	Mult _, TInt, TInt -> TInt
	|	Eq _, TInt, TInt -> TBool
	|	Eq _, TBool, TBool -> TBool
	|	LEq _, TInt, TInt -> TBool
	|	GEq _, TInt, TInt -> TBool
	|	And _, TBool, TBool -> TBool
	|	Or _, TBool, TBool -> TBool
	|	t, t1t, t2t -> raise (TypeError("Type mismatch on " ^ (termToString t) ^ " operation, provided: " ^ (termTypeToString t1t) ^ " " ^ (termTypeToString t2t)))

and typeOfLet env n nType t1 t2 =
	let t1Type = typeof env t1 in
	if nType = t1Type then
		let env' = envAssign env n nType in
		typeof env' t2
	else
		raise (TypeError("Type mismatch on Let, variable type: " ^ (termTypeToString nType) ^ ", provided expression type: " ^ (termTypeToString t1Type)))

let exp = Let("x", TInt, Int(2), Sum(Var("x"), Var("x")));;
let expType = typeof env exp;;
print_endline ("Expression Type: " ^ (termTypeToString expType));;
print_endline "Type Checking Complete";;

