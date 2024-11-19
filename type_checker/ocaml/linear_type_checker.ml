
exception TypeError of string

type termType =
|   TInt
|	TBool
|	TTList of termType * termType

type term =
|   Int of int
|	Bool of bool
|	LVar of string
|	Let of string * term * term (* let x : t = exp1 in exp2 *)
|	LDisj of term * term
|	LConj of term * term
|	LAltConj of term * term
|   Func of string * termType * term (* paramName paramType expBody *)
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
	|	TTList _ -> "TTList"

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
	|	Int(i) -> env, TInt
	|	Bool(b) -> env, TBool
	|	LVar(n) -> env, envLookup env n
	|	Let(n, t1, t2) -> typeofLet env n t1 t2
	|	LDisj(t1, t2)
	|	LConj(t1, t2)
	|	LAltConj(t1, t2) -> typeofLPair env exp t1 t2
	|	Func(pName, pType, b) -> typeofFunc env pName pType b
	|	FuncCall(t1, t2) -> typeofFuncCall env t1 t2

and typeofLet env n t1 t2 =
	let env', t1Type = typeof env t1 in
	let env'' = envNewBind env' n t1Type in
	typeof env'' t2

and typeofLPair env op t1 t2 =
	match t1, t2 with
	|	LVar(n1), LVar(n2) ->
			let _, t1t = typeof env t1 in 
			let env' = envRemoveBind env n1 in
			let _, t2t = typeof env' t2 in
			let env'' = envRemoveBind env' n2 in
			if (=) t1t t2t then env'', t1t
			else raise (TypeError("Type mismatch on " ^ (termToString op) ^ " operation, provided: " ^ (termTypeToString t1t) ^ " " ^ (termTypeToString t2t)))
	|	_ -> raise (TypeError("Must pass LVar parameters on " ^ (termToString op) ^ " operation, provided: " ^ (termToString t1) ^ " " ^ (termToString t2)))

and typeofFunc env pName pType b =
	let env' = envNewBind env pName pType in 
	let env'', bType = typeof env' b in
	env'', TTList(pType, bType)

and typeofFuncCall env t1 t2 =
	let env', t1Type = typeof env t1 in
	let env'', t2Type = typeof env' t2 in
	match t1Type with
	|	TTList(fType1, fType2) ->
			if (=) fType1 t2Type then env'', fType2
			else raise (TypeError("Type mismatch on FuncCall parameter, expected: " ^ (termTypeToString fType1) ^ ", provided: " ^ (termTypeToString t2Type)))
	|	_ -> raise (TypeError("Arrow function type expected"))


let exp = 
	Let("x", Int(2), 
	Let("y", Int(4),
		FuncCall(Func("x", TInt, LDisj(LVar("x"), LVar("y"))), LVar("x"))
	))
;;


(*
let exp = 
	Let("y", Int(4),
		FuncCall(Func("x", TInt, LDisj(LVar("x"), LVar("y"))), Int(1))
	)
;;
*)

let rec printEnv env = 
	match env with
	| [] -> ()
	| (n, t)::env' -> print_string (n ^ ":" ^ (termTypeToString t)); print_string ", "; printEnv env';;

let env, expType = typeof env exp;;

print_endline ("Expression Type: " ^ (termTypeToString expType));;
if not (List.is_empty env) then begin
	print_string "Unused resources: "; 
	printEnv env;
	print_endline ""
end;;
print_endline "Type Checking Complete";;
