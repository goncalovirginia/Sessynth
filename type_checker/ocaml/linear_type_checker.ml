
exception TypeError of string

type termType =
|   TInt
|	TBool
|	TTypeList of termType * termType

type term =
|   Int of int
|	Bool of bool
|	LVar of string
|	Let of string * termType * term * term (* let x : t = exp1 in exp2 *)
|	LDisj of term * term
|	LConj of term * term
|	LAltConj of term * term
|   Func of string * termType * term (* fName param1Type expBody *)
|   FuncCall of term * term (* expFunc expParam *)

let termToString term =
	match term with
	|	Int _ -> "Int"
	|	Bool _ -> "Bool"
	|	LVar _ -> "LVar"
	|	Let _ -> "Let"
	|	LDisj _ -> "LDisj"
	|	LConj _ -> "LConj"
	|	LAltConj _ -> "LAltConj"
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

let envNewBind env n nType =
	(n, nType) :: env

let envReplaceBind env n nType =
	(n, nType) :: List.remove_assoc n env

let envRemoveBind env n =
	List.remove_assoc n env

let rec typeof env exp =
    match exp with
	|	Int(i) -> TInt
	|	Bool(b) -> TBool
	|	LVar(n) -> envLookup env n
	|	Let(n, nType, t1, t2) -> typeofLet env n nType t1 t2
	|	LDisj(t1, t2)
	|	LConj(t1, t2)
	|	LAltConj(t1, t2) -> typeofLPair env exp t1 t2
	|	Func(n, paramType, b) -> typeofFunc env n paramType b
	|	FuncCall(f, t1) -> typeofFuncCall env f t1

and typeofLPair env op t1 t2 =
	match t1, t2 with
	|	LVar(n1), LVar(n2) ->
			let t1t = typeof env t1 in 
			let env' = envRemoveBind env n1 in
			let t2t = typeof env' t2 in
			if (=) t1t t2t then t1t
			else raise (TypeError("Type mismatch on " ^ (termToString op) ^ " operation, provided: " ^ (termTypeToString t1t) ^ " " ^ (termTypeToString t2t)))
	|	_ -> raise (TypeError("Must pass LVar parameters on " ^ (termToString op) ^ " operation, provided: " ^ (termToString t1) ^ " " ^ (termToString t2)))

and typeofLet env n nType t1 t2 =
	let t1Type = typeof env t1 in
	if nType = t1Type then
		let env' = envNewBind env n nType in
		typeof env' t2
	else
		raise (TypeError("Type mismatch on Let, expected: " ^ (termTypeToString nType) ^ ", provided: " ^ (termTypeToString t1Type)))

and typeofFunc env n paramType b =
	let env' = envNewBind env n paramType in 
	let bType = typeof env' b in
	TTypeList(paramType, bType)

and typeofFuncCall env f t1 =
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

let exp = 
	Let("x", TInt, Int(2), 
		Let("y", TInt, Int(4),
			FuncCall(Func("f", TInt, LDisj(LVar("x"), LVar("y"))), LVar("x"))
		)
	)
;;

let expType = typeof env exp;;
print_endline ("Expression Type: " ^ (termTypeToString expType));;
print_endline "Type Checking Complete";;
