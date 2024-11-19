
exception TypeError of string
exception NoRuleApplies

type expType =
|   TInt
|	TBool
|	TTypeList of expType * expType

type exp =
|   Int of int
|	Bool of bool
|	LVar of string
|	Let of string * exp * exp (* let x : t = exp1 in exp2 *)
|	LDisj of exp * exp
|	LConj of exp * exp
|	LAltConj of exp * exp
|   Func of string * expType * exp (* fName param1Type expBody *)
|   FuncCall of exp * exp (* expFunc expParam *)

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
	| Int(i) -> TInt
	| Bool(b) -> TBool
	| LVar(n) -> envLookup env n	
	| _ -> raise (TypeError("isval should've prevented this"))

let rec isval env exp =
	match exp with
	| Int _ 
	| Bool _ 
	| LVar _ -> true
	|	_ -> false

let rec eval1 env exp =
	match exp with
	| Let(n, e1, e2) when isval env e1 -> 
		let env' = envNewBind env n (typeof env e1) 
		in env', e2
	| Let(n, e1, e2) -> 
		let env', e1' = eval1 env e1 in 
		env', Let(n, e1', e2)
	| LDisj(LVar(n1), LVar(n2)) -> 
		let n1Type = envLookup env n1 in
		let env' = envRemoveBind env n1 in
		let n2Type = envLookup env' n2 in
		let env' = envRemoveBind env n2 in
		if (=) n1Type n2Type then env', Int(1)
		else env', Int(2)
	| Func(n, pType, b) -> 
		env, Int(1)
	| _ -> raise NoRuleApplies

let rec eval env exp =
	try let env', exp' = eval1 env exp in eval env' exp'
	with NoRuleApplies -> exp

(** 
let x = 2 in
(fun x -> x + 2) x;;
**)

let exp = 
	Let("x", Int(2), 
		Let("y", Int(4),
			FuncCall(Func("f", TInt, LDisj(LVar("x"), LVar("y"))), LVar("x"))
		)
	)
;;

let expType = typeof env exp;;
print_endline ("Expression Type: " ^ (termTypeToString expType));;
print_endline "Type Checking Complete";;
