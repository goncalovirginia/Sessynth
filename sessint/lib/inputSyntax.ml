(* This syntax is merely meant to serve as an intermediate representation 
   between input and the actual syntax used in the type checking process.
   *)

type bop = 
  | Mul 
  | Div 
  | Add
  | Sub 
  | And 
  | Or 
  | Lesser 
  | Greater
  | Equals 

type uop = 
  | Not 
  | Neg

type var = string

(* TVar is meant to represent the variable that represents a language type
   STUVar represents the variable that representas a session type
   STVar represents the recursion variable
   *)
type ty =
  | TUnit
  | TNum
  | TBool
  | TFun of ty list * ty
  | TProc of stype * (var * stype) list
  | TVar of var
and stype =
  | STSend of ty * stype
  | STRecv of ty * stype
  | STEnd 
  | STExtChoice of (var * stype) list
  | STIntChoice of (var * stype) list
  | STSendChan of stype * stype
  | STRecvChan of stype * stype
  | STVar of var
  | STRec of var * stype
  | STUVar of var

type exp = 
  | UnitVal 
  | Num of int
  | Bool of bool
  | Var of var
  | BOp of bop * exp * exp
  | UOp of uop * exp
  | Let of var * exp * exp
  | FunDef of ((var list) * ty option) list * exp 
  | FunApp of exp * (exp list)
  | Annot of exp * ty
  | Cond of exp * exp * exp
  | ProcExp of var * proc
  | ExecExp of exp
and proc = 
  | Send of var * exp * proc
  | Recv of var * var * ty option * proc
  | Close of var
  | Wait of var * proc
  | Fwd of var * var
  | Spawn of var * exp * proc * var list
  | Choice of var * (var * proc) list
  | Label of var * var * proc
  | SendChan of var * var * proc
  | RecvChan of var * var * stype option * proc 
  | Print of exp * proc
  | If of exp * proc * proc

type decl = 
  | Decl of var * exp

type prog = 
  | Prog of (decl list) * exp


 

  