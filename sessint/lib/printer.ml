open Syntax
(* This module is just meant to ease the printing of the abstract syntax tree.
   There are also the various error definitions used in the type checker.
   *)

type type_err = 
  | NoSuchArg of Syntax.var
  | UnexpectedType of Syntax.ty * Syntax.ty (* Got first ty, expected second ty *)
  | NotFunctionType of Syntax.ty 
  | CannotInferType
  | NotBinaryOp of Syntax.exp
  | NotUnaryOp of Syntax.exp
  | NonEmptyLinearContext
  | UnexpectedCheckedSType of Syntax.stype 
  | UnexpectedSType of Syntax.stype * Syntax.stype (* Got first ty, expected second ty *)
  | NoSuchChannelInContext of Syntax.var
  | ChannelHasWrongSType of Syntax.var * Syntax.stype
  | CannotWaitOwnChannel of Syntax.var
  | CannotWaitChannelOfType of Syntax.var * Syntax.stype 
  | CannotCloseUnownedChannel of Syntax.var
  | NonMatchingSTypes of Syntax.stype * Syntax.stype
  | InvalidForwardChannel of Syntax.var
  | ChannelAlreadyExists of Syntax.var
  | NotProcessType of Syntax.ty
  | EmptyChoice
  | NoSuchLabelInType of Syntax.var * Syntax.stype
  | AllCasesMustProduceIdenticalCtxt
  | NoCaseForLabel of Syntax.var
  | AllCasesMustProduceIdenticalType of Syntax.stype * Syntax.stype
  | NonExecutableExpression of Syntax.exp
  
exception TError of type_err

let error err = raise (TError err)

let string_from_binary_op op = 
  match op with 
  | Syntax.Add -> " + "
  | Syntax.Sub -> " - "
  | Syntax.Mul -> " * " 
  | Syntax.Div -> " / "
  | Syntax.Or -> " v "
  | Syntax.And -> " ^ "
  | Syntax.Equals -> " = "
  | Syntax.Lesser -> " < "
  | Syntax.Greater -> " > "

let string_from_unary_op op = 
   match op with 
   | Syntax.Not -> "not "
   | Syntax.Neg -> "- "

let rec string_from_type ty = 
  match ty with 
  | Syntax.TUnit -> "unit"
  | Syntax.TBool -> "bool"
  | Syntax.TNum -> "int"
  | Syntax.TFun (t1, t2) -> "(" ^ (string_from_type t1) ^ " -> " ^ (string_from_type t2) ^ ")"
  | Syntax.TProc (st, ctxt) -> "{ " ^ (string_from_stype st) ^ " <- " ^ string_from_lin_ctxt ctxt ^ " }"
  | Syntax.TVar v -> v

and string_from_lin_ctxt ctxt = 
  match ctxt with 
  | [(c, st)] ->  c ^ " : " ^ string_from_stype st
  | (c, st)::tail -> c ^ " : " ^ string_from_stype st ^ ", " ^ string_from_lin_ctxt tail
  | [] -> ""

and string_from_stype stype = 
  match stype with 
  | Syntax.STSend (t, st) -> (string_from_type t) ^ " ^ " ^ (string_from_stype st)
  | Syntax.STRecv (t, st) -> (string_from_type t) ^ " => " ^ (string_from_stype st)
  | Syntax.STEnd -> "@"
  | Syntax.STExtChoice l -> "&{ " ^ (string_from_choice_type_list l) ^ "}"
  | Syntax.STIntChoice l -> "+{ " ^ (string_from_choice_type_list l) ^ "}"
  | Syntax.STSendChan (sent, st) -> "(" ^ (string_from_stype sent) ^ ") * " ^ (string_from_stype st)
  | Syntax.STRecvChan (recvd, st) -> "(" ^ (string_from_stype recvd) ^ ") -o " ^ (string_from_stype st)
  | Syntax.STRec (x, st) -> "rec " ^ x ^ "." ^ (string_from_stype st)
  | Syntax.STVar v -> "var " ^ v
  | Syntax.STUVar v -> v

and string_from_choice_type_list l =
  match l with 
  | [(v, st)] -> v ^ " : " ^ (string_from_stype st)
  | (v, st)::ls -> v ^ " : " ^ (string_from_stype st) ^ ", " ^ (string_from_choice_type_list ls)
  | [] -> ""

let string_from_ty_option opt = 
  match opt with
  | None -> ""
  | Some t -> " : " ^ string_from_type t

let string_from_stype_option opt = 
  match opt with
  | None -> ""
  | Some st -> " : " ^ string_from_stype st


let rec string_from_exp e = 
  match e with 
  | Syntax.UnitVal -> "()"
  | Syntax.Num i -> string_of_int i
  | Syntax.Bool b -> string_of_bool b
  | Syntax.Var v -> v
  | Syntax.BOp (op, e1, e2) -> "(" ^string_from_exp e1 ^ string_from_binary_op op ^ string_from_exp e2 ^ ")"
  | Syntax.UOp (op, e1) -> "(" ^ string_from_unary_op op ^ string_from_exp e1 ^ ")"
  | Syntax.Let (var, e1, e2) -> "let " ^ var ^ " = " ^ string_from_exp e1 ^ " in " ^ string_from_exp e2
  | Syntax.FunDef (x, opt, body, ret_type) -> "fun " ^ x ^ string_from_ty_option opt ^ " -> " ^ string_from_exp body ^ " end " ^ string_from_ty_option ret_type
  | Syntax.FunApp (f, arg) -> "app" ^ "(" ^ string_from_exp f ^ ", " ^ string_from_exp arg ^ ")"
  | Syntax.Annot (e', t) -> "(" ^ (string_from_exp e') ^ " : " ^ (string_from_type t) ^ ")"
  | Syntax.Cond (cond, e1, e2) -> "if " ^ (string_from_exp cond) ^ " then " ^ (string_from_exp e1) ^ " else " ^ (string_from_exp e2) ^ " endif"
  | Syntax.ProcExp (c, p, o, ctxt) -> c ^ " : { " ^ (string_from_proc p) ^ " }" ^ (string_from_stype_option o) ^ " <- " ^ string_from_lin_ctxt ctxt
  | Syntax.ExecExp exp -> "exec ( " ^ (string_from_exp exp) ^ " )" 

  and string_from_proc p =
  match p with
  | Syntax.Send (c, e, o, p) -> "send " ^ c ^ " " ^ (string_from_exp e) ^ (string_from_ty_option o) ^ " ; " ^ (string_from_proc p)
  | Syntax.Recv (v, c, t, p) -> v ^ (string_from_ty_option t) ^ " <- recv " ^ c ^ " ; " ^ (string_from_proc p)
  | Syntax.Close (c) -> "close " ^ c
  | Syntax.Wait (c, p) -> "wait " ^ c ^ " ; " ^ (string_from_proc p)
  | Syntax.Fwd (st, c1, c2) -> "fwd " ^ c1 ^ " " ^ c2 ^ (string_from_stype_option st)
  | Syntax.Spawn (c, e, _, p', l) -> c ^ " <- spawn " ^ (string_from_exp e) ^ (string_from_var_list l) ^ " ; " ^ (string_from_proc p')
  | Syntax.Choice (c, l) -> "case " ^ c ^ " of " ^ " ( " ^ (string_from_choice_list l) ^ " ) "
  | Syntax.Label (c, l, p, opt) -> c ^ "." ^ l ^ string_from_stype_option opt ^ " ; " ^ (string_from_proc p)
  | Syntax.SendChan (c, e, o, p) -> "sendc " ^ c ^ " " ^ e ^  (string_from_stype_option o) ^ " ; " ^ (string_from_proc p)
  | Syntax.RecvChan (v, c, st, p) -> v ^ (string_from_stype_option st) ^ " <- recvc " ^ c ^ " ; " ^ (string_from_proc p)
  | Syntax.Print (e, p) -> "print " ^ (string_from_exp e) ^ " ; " ^ (string_from_proc p)
  | Syntax.If (e, p1, p2) -> "if " ^ (string_from_exp e) ^ " then " ^ (string_from_proc p1) ^ " else " ^ (string_from_proc p2) 

and string_from_var_list l = 
  match l with
  | v::tail -> v ^ " " ^ string_from_var_list tail
  | [] -> ""

and string_from_choice_list cl = 
  match cl with 
  | [(v, (p, o))] -> v ^ " => " ^ (string_from_proc p) ^ (string_from_stype_option o) 
  | (v, (p, o))::rest -> v ^ " => " ^ (string_from_proc p) ^ (string_from_stype_option o) ^ " , " ^ (string_from_choice_list rest)
  | [] -> ""

let string_from_decl decl = 
  match decl with
  | Syntax.Decl (var, ty, exp) -> var ^ " = " ^ "(" ^ (string_from_exp exp) ^ " : " ^ (string_from_type ty) ^ ")"

let string_from_err err = 
  match err with
  | NoSuchArg arg -> "NoSuchArg: " ^ arg 
  | NotFunctionType ty -> "NotFunctionType: " ^ (string_from_type ty)
  | UnexpectedType (t1, t2) -> "UnexpectedType: " ^ "got " ^ (string_from_type t1) ^ ", expected " ^ (string_from_type t2)
  | CannotInferType -> "CannotInferType"
  | NotBinaryOp e -> "NotBinaryOp: " ^ (string_from_exp e)
  | NotUnaryOp e -> "NotUnaryOp: " ^ (string_from_exp e)
  | NonEmptyLinearContext -> "NonEmptyLinearContext"
  | UnexpectedCheckedSType st -> "UnexpectedStype: " ^ (string_from_stype st)
  | UnexpectedSType (st1, st2) -> "UnexpectedSType: " ^ "got " ^ (string_from_stype st1) ^ ", expected " ^ (string_from_stype st2) 
  | NoSuchChannelInContext c -> "NoSuchChannelInContext: " ^ c
  | ChannelHasWrongSType (c, st) -> "ChannelHasWrongSType: channel " ^ c ^ " of type " ^ (string_from_stype st)
  | CannotWaitOwnChannel c -> "CannotWaitOwnChannel: " ^ c
  | CannotWaitChannelOfType (c, st) -> "CannotWaitChannelOfType: " ^ c ^ ":" ^ (string_from_stype st)
  | CannotCloseUnownedChannel c -> "CannotCloseUnownedChannel: " ^ c
  | NonMatchingSTypes (st1, st2) -> "NonMatchingSTypes: " ^ (string_from_stype st1) ^ " and " ^ (string_from_stype st2)
  | InvalidForwardChannel c -> "InvalidForwardChannel: " ^ c
  | ChannelAlreadyExists c -> "ChannelAlreadyExists: " ^ c
  | NotProcessType ty -> "NotProcessType: " ^ (string_from_type ty)
  | EmptyChoice -> "EmptyChoice"
  | NoSuchLabelInType (v, st) -> "NoSuchLabelInType: " ^ v ^ " in " ^ (string_from_stype st)
  | AllCasesMustProduceIdenticalCtxt -> "AllCasesMustProduceIdenticalCtxt"
  | NoCaseForLabel v -> "NoCaseForLabel: " ^ v
  | AllCasesMustProduceIdenticalType (st1, st2) -> "AllCasesMustProduceIdenticalType: got " ^ (string_from_stype st1) ^ ", expected " ^ string_from_stype st2
  | NonExecutableExpression exp -> "NonExecutableExpression: " ^ string_from_exp exp