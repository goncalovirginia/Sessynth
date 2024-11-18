
exception TypeError of string

type termType =
|   TInt
|	TBool
|	TTypeList of termType * termType

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
|   FuncCall of term * term (* expFunc expParam *)

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
	|	TTypeList _ -> "TTypeList"

let env = []

let envLookup env n =
	List.assoc n env

let envBind env n nType =
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
	|	Func(n, paramType, b) -> typeOfFunc env n paramType b
	|	FuncCall(f, t1) -> typeOfFuncCall env f t1

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
		let env' = envBind env n nType in
		typeof env' t2
	else
		raise (TypeError("Type mismatch on Let, expected: " ^ (termTypeToString nType) ^ ", provided: " ^ (termTypeToString t1Type)))

and typeOfFunc env n paramType b =
	let env' = envBind env n paramType in 
	let bType = typeof env' b in
	TTypeList(paramType, bType)

and typeOfFuncCall env f t1 =
	let fType = typeof env f in
	let t1Type = typeof env t1 in
	match fType with
	|	TTypeList(fType1, fType2) ->
			if (=) fType1 t1Type then fType2
			else raise (TypeError("Type mismatch on FuncCall parameter, expected: " ^ (termTypeToString fType1) ^ ", provided: " ^ (termTypeToString t1Type)))
	|	_ -> raise (TypeError("Arrow function type expected"))

(** 
let x = 2 in
(fun x -> x + 2) x;;
**)

let exp = Let("x", TInt, Int(2), 
	FuncCall(Func("f", TInt, Sum(Var("x"), Int(2))), Var("x"))
);;

let expType = typeof env exp;;
print_endline ("Expression Type: " ^ (termTypeToString expType));;
print_endline "Type Checking Complete";;
