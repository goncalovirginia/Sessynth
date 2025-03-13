(* Syntax terms are used to build the abstract syntax tree of the language
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

type ty =
 | TUnit
 | TNum
 | TBool
 | TFun of ty * ty
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
 | FunDef of var * ty option * exp * ty option (* fun x:t -> .... OU fun x -> .... *)
 | FunApp of exp * exp 
 | Annot of exp * ty
 | Cond of exp * exp * exp
 | ProcExp of var * proc * stype option * (var * stype) list
 | ExecExp of exp
and proc = 
  | Send of var * exp * ty option * proc
  | Recv of var * var * ty option * proc  (* x <- recv c ; P  OU   (x:t) <- recv c ; P *)
  | Close of var      (*  nada |-  close c :: c:STEnd *)
  | Wait of var * proc (*  c:STEnd |- wait c ; P :: d:T *)
  | Fwd of stype option * var * var
  | Spawn of var * exp * stype option * proc * var list
  | Choice of var * (var * (proc * stype option)) list
  | Label of var * var * proc * stype option
  | SendChan of var * var * stype option * proc
  | RecvChan of var * var * stype option * proc 
  | Print of exp * proc
  | If of exp * proc * proc

type decl = 
 | Decl of var * ty * exp

type prog = 
 | Prog of (decl list) * exp

 (* Desugaring functions, change from input syntax to regular syntax *)
 let rec desugar_ty t = 
  match t with 
  | InputSyntax.TUnit -> TUnit
  | InputSyntax.TBool -> TBool
  | InputSyntax.TNum -> TNum
  | InputSyntax.TFun (ts, ret) -> 
    begin match ts with 
    | h::tail -> TFun (desugar_ty h, desugar_ty (InputSyntax.TFun (tail, ret)))
    | [] -> desugar_ty ret
    end
  | InputSyntax.TProc (st, ctxt) -> TProc (desugar_stype st, desugar_lin_ctxt ctxt)  
  | InputSyntax.TVar v -> TVar v
and desugar_lin_ctxt ctxt = 
  match ctxt with 
  | (c, st)::tail -> (c, desugar_stype st)::(desugar_lin_ctxt tail)
  | [] -> []
and desugar_ty_opt opt = 
  match opt with 
  | Some ty -> Some (desugar_ty ty);
  | None -> None
and desugar_stype st = 
  match st with
  | InputSyntax.STSend (t, st') -> STSend (desugar_ty t, desugar_stype st')
  | InputSyntax.STRecv (t, st') -> STRecv (desugar_ty t, desugar_stype st')
  | InputSyntax.STEnd -> STEnd
  | InputSyntax.STExtChoice l -> STExtChoice (desugar_choice_type_list l)
  | InputSyntax.STIntChoice l -> STIntChoice (desugar_choice_type_list l)
  | InputSyntax.STSendChan (sent, st') -> STSendChan (desugar_stype sent, desugar_stype  st')
  | InputSyntax.STRecvChan (recvd, st') -> STRecvChan (desugar_stype recvd, desugar_stype st')
  | InputSyntax.STVar v -> STVar v
  | InputSyntax.STRec (v, p) -> STRec (v, desugar_stype p)
  | InputSyntax.STUVar v -> STUVar v

and desugar_choice_type_list list =
  match list with 
  | (v, st)::ls -> (v, desugar_stype st)::(desugar_choice_type_list ls)
  | [] -> []

let desugar_stype_opt opt = 
  match opt with 
  | Some ty -> Some (desugar_stype ty);
  | None -> None

let desugar_bop op = 
  match op with
  | InputSyntax.Mul-> Mul
  | InputSyntax.Div -> Div
  | InputSyntax.Add-> Add
  | InputSyntax.Sub -> Sub
  | InputSyntax.And -> And
  | InputSyntax.Or -> Or
  | InputSyntax.Lesser -> Lesser
  | InputSyntax.Greater-> Greater
  | InputSyntax.Equals -> Equals 

let desugar_uop op = 
  match op with 
  | InputSyntax.Neg -> Neg
  | InputSyntax.Not -> Not

let rec desugar_exp se = 
  match se with 
  | InputSyntax.UnitVal -> UnitVal
  | InputSyntax.Num n -> Num n
  | InputSyntax.Bool b -> Bool b
  | InputSyntax.Var v -> Var v
  | InputSyntax.BOp (op, e1, e2) -> BOp (desugar_bop op, desugar_exp e1, desugar_exp e2)
  | InputSyntax.UOp (op, e) -> UOp (desugar_uop op, desugar_exp e)
  | InputSyntax.Let (v, e1, e2) -> Let (v, desugar_exp e1, desugar_exp e2)
  | InputSyntax.FunDef (l, e) -> let params = desugar_fun_params l in
                                  List.fold_right (fun (x, ty) acc -> FunDef (x, desugar_ty_opt ty, acc, None)) params (desugar_exp e)
  | InputSyntax.FunApp (e, el) -> List.fold_left (fun acc x -> FunApp (acc, desugar_exp x)) (desugar_exp e) el
  | InputSyntax.Annot (e, ty) -> Annot (desugar_exp e, desugar_ty ty)
  | InputSyntax.Cond (cond, e1, e2) -> Cond (desugar_exp cond, desugar_exp e1, desugar_exp e2)
  | InputSyntax.ProcExp (c, proc) -> ProcExp (c, desugar_proc proc, None, [])
  | InputSyntax.ExecExp e -> ExecExp (desugar_exp e)

and desugar_fun_params l = 
  match l with 
  | p::tail -> desugar_fun_params_aux p @ desugar_fun_params tail
  | [] -> []

and desugar_fun_params_aux pair =
  match pair with 
  | (first::rest, ty) -> (first, ty) :: desugar_fun_params_aux (rest, ty)
  | ([], _) -> []

and desugar_proc proc = 
  match proc with 
  | InputSyntax.Send (c, e, p) -> Send (c, desugar_exp e, None, desugar_proc p)
  | InputSyntax.Recv (v, c, t, p) -> Recv (v, c, desugar_ty_opt t, desugar_proc p) 
  | InputSyntax.Close (c) -> Close (c)
  | InputSyntax.Wait (c, p) -> Wait (c, desugar_proc p)
  | InputSyntax.Fwd (c1, c2) -> Fwd (None, c1, c2) 
  | InputSyntax.Spawn (c, e, p, a) -> Spawn (c, desugar_exp e, None, desugar_proc p, a)
  | InputSyntax.Choice (c, l) -> Choice (c, desugar_choice_list l)
  | InputSyntax.Label (c, l, p) -> Label (c, l, desugar_proc p, None)
  | InputSyntax.SendChan (c, e, p) -> SendChan (c, e, None, desugar_proc p)
  | InputSyntax.RecvChan (v, c, st, p) -> RecvChan (v, c, desugar_stype_opt st, desugar_proc p)
  | InputSyntax.Print (e, p) -> Print (desugar_exp e, desugar_proc p)
  | InputSyntax.If (e, p1, p2) -> If (desugar_exp e, desugar_proc p1, desugar_proc p2)
and desugar_choice_list list = 
  match list with
  | (v, p)::rest -> (v, (desugar_proc p, None))::(desugar_choice_list rest)
  | [] -> []

let desugar_decl decl = 
  match decl with
  | InputSyntax.Decl (var, Annot (exp, ty)) -> Decl (var, desugar_ty ty, desugar_exp exp)
  | _ -> assert false (* should never happen *)

let desugar_prog prog = 
  match prog with 
  | InputSyntax.Prog (ldecls, exp) -> Prog (List.rev (List.fold_left (fun acc d -> (desugar_decl d)::acc) [] ldecls), desugar_exp exp)