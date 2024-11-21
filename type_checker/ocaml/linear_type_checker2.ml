exception TypeError of string
exception BindingError of string

(* Type definitions *)

type termType =
|	TInt
|	TBool
|	TTList of termType list

| MPair of termType * termType 
| APair of termType * termType 
| LSum of termType * termType
| LFun of termType list * termType 

type term =
|	Int of int
|	Bool of bool
|	LVar of string
|	Let of string * term * term (* let x = exp1 in exp2 *)

(* A \oplus B *)
|	Inl of term * termType (* M:A entao Inl_B M : A + B *)
| Inr of term * termType (* M:B entao Inr_A M : A + B *)
| Case of term * string * term * string * term (* case M of inl x -> E1 , inr x -> E2 *)

(* A \otimes B *)
|	LConj of term * term (* Par multiplicativo: (M1 * M2) : A * B *)
| LLet of term * string * string * term (* let x*y = M in N  *)

(* A & B *)
|	LAltConj of term * term (* Par aditivo: ( M1 & M2 ) *)
| Fst of term (* fst (M1,M2) -> M1 *)
| Snd of term (* snd (M1,M2) -> M2 *)

(* (A1,...,An) -o B *)
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
	|	LDisj _ -> "LDisj"
	|	LConj _ -> "LConj"
	|	LAltConj _ -> "LAltConj"
	|	Func _ -> "Func"
	|	FuncCall _ -> "FuncCall"

let rec termTypeToString termType =
	match termType with
	|	TInt -> "TInt"
	|	TBool -> "TBool"
	|	TTList(termTypeList) -> "TTList(" ^ (termTypeListToString termTypeList) ^ ")"

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
	| Inl(e,t) -> begin 
									let env , t1 = typeof env e in 
									env , LSum(t1,t) 
								end
  | Inr(e,t) -> begin 
									let env , t2 = typeof env e in 
										env , LSum(t,t2) 
								end
	| Case(e,x,el,y,er) -> begin 
				let env , LSum(t1,t2) = typeof env e in 
				(* tipificar el em que x:t1, tipificar er em que y:t2 e 
				    tipo de el e er iguais*)
						end
  | LConj(e1,e2) -> 
		  let env' , t1 = typeof env e1 in 
			let env'' , t2 = typeof env' e2 in
       env'', MPair(t1,t2)
	| LLet(e1,x,y,e2) -> 
		  (* *)
			begin 
			let env , MPair(t1,t2) = typeof env e1 in
					(* tipificar e2 num ambiente em que x:t1,y:t2 *)
      end

	|	LAltConj(e1, e2) -> 
		begin
			let env1 , t1 = typeof env e1 in 
			let env2 , t2 = typeof env e2 in 
			(* Verificar algo sobre env1 e env2 *)
			env2,APair(t1,t2)
		end 
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
