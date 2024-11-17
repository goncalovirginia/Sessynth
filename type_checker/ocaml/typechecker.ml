
exception TypeError of string

type term =
|   Int of int
|	Bool of bool
|	Sum of term * term
|	Mult of term * term
|	Eq of term * term
|	LEq of term * term
|	GEq of term * term
|	And of term * term
|	Or of term * term
|   Func of term * term * term
|   FuncCall of term * term

let termToString term =
	match term with
	|	Int _ -> "Int"
	|	Bool _ -> "Bool"
	|	Sum _ -> "Sum"
	|	Mult _ -> "Mult"
	|	Eq _ -> "Eq"
	|	LEq _ -> "LEq"
	|	GEq _ -> "GEq"
	|	And _ -> "And"
	|	Or _ -> "Or"
	|	Func _ -> "Func"
	|	FuncCall _ -> "FuncCall"

type termType =
|   TInt
|	TBool

let termTypeToString termType =
	match termType with
	|	TInt -> "TInt"
	|	TBool -> "TBool"

let env = []

let envLookup env exp =
	List.assoc exp env

let rec typeof env exp =
    match exp with
	|	Int(i) -> TInt
	|	Bool(b) -> TBool
	|	Sum(t1, t2)
	|	Mult(t1, t2)
	|	Eq(t1, t2)
	|	LEq(t1, t2)
	|	GEq(t1, t2)
	|	And(t1, t2)
	|	Or(t1, t2) -> typeofPairOp env exp t1 t2
	|	Func(f, t1, b) -> typeof env b
	|	FuncCall(f, t1) -> typeof env f (*let funcTypes = envLookup env f in let funcArgType = typeof env t1 in
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

let typecheck exp =
	typeof env exp

let exp = Sum(Int(2), Int(2));;
let expType = typecheck exp;;
print_endline ("Expression Type: " ^ (termTypeToString expType));;
print_endline "Type Checking Complete";;

