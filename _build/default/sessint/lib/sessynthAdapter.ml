open Syntax
open Sessynth

(* sessint -> sessynth *)

let rec ty_to_tyF ty =
    match ty with
    | TUnit -> assert false
    | TNum -> Language.TAtomic(TInt)
    | TBool -> Language.TAtomic(TBool)
    | TProc(st, l) -> Language.TProcess(List.map(fun (_, s) -> stype_to_tyS s) l, stype_to_tyS st)
    | TFun(t1, t2) -> Language.TArrow(ty_to_tyF t1, ty_to_tyF t2)
    | TVar(x) -> Language.TDeclr(x)

and stype_to_tyS sty =
  match sty with
  | STEnd -> Language.STUnit
  | STExtChoice l -> Language.STExtChoice(List.map(fun (l, s) -> (l, stype_to_tyS s)) l)
  | STIntChoice l -> Language.STIntChoice(List.map(fun (l, s) -> (l, stype_to_tyS s)) l)
  | STSend(t, st) -> Language.STSendF(ty_to_tyF t, stype_to_tyS st)
  | STRecv(t, st) -> Language.STRecvF(ty_to_tyF t, stype_to_tyS st)
  | STSendChan(st1, st2) -> Language.STSendS(stype_to_tyS st1, stype_to_tyS st2)
  | STRecvChan(st1, st2) -> Language.STRecvS(stype_to_tyS st1, stype_to_tyS st2)
  | STRec(v, st) -> Language.STRec(v, stype_to_tyS st)
  | STVar(x) -> Language.STRecVar(x)
  | STUVar(x) -> Language.STDeclr(x)
  | STMultiSend _ | STMultiRecv _ -> assert false 

(* sessynth -> sessint *)

let sessynth_uop_to_uop op =
    match op with
    | Language.Not -> Not
    | Language.Neg -> Neg

let sessynth_bop_to_bop op =
    match op with
    | Language.And -> And
    | Language.Or -> Or
    | Language.Eq -> Equals
    | Language.Gr -> Greater
    | Language.Lt -> Lesser
    | Language.Sum -> Add
    | Language.Sub -> Sub
    | Language.Mult -> Mul
    | Language.Div -> Div
    | _ -> assert false

let tyA_to_ty tyA =
    match tyA with
    | Language.TInt -> TNum
    | Language.TBool -> TBool

let rec tyF_to_ty tyF =
    match tyF with
    | Language.TAtomic tA -> tyA_to_ty tA
    | Language.TRefinement _ -> assert false
    | Language.TArrow(tF1, tF2) -> TFun(tyF_to_ty tF1, tyF_to_ty  tF2)
    | Language.TProcess(tSl, tS) -> TProc(tyS_to_stype tS, List.map(fun tS -> ("c", tyS_to_stype tS)) tSl)
    | Language.TDeclr(x) -> TVar(x)

and tyS_to_stype tyS =
    match tyS with
    | Language.STSendF(tF, tS) -> STSend(tyF_to_ty tF, tyS_to_stype tS)
    | Language.STRecvF(tF, tS) -> STRecv(tyF_to_ty tF, tyS_to_stype tS)
    | Language.STSendS(tS1, tS2) -> STSendChan(tyS_to_stype tS1, tyS_to_stype tS2)
    | Language.STRecvS(tS1, tS2) -> STRecvChan(tyS_to_stype tS1, tyS_to_stype tS2)
    | Language.STUnit -> STEnd
    | Language.STExtChoice labelsesslist -> STExtChoice(List.map(fun (l, s) -> (l, tyS_to_stype s)) labelsesslist)
    | Language.STIntChoice labelsesslist -> STIntChoice(List.map(fun (l, s) -> (l, tyS_to_stype s)) labelsesslist)
    | Language.STRec(t, tS) -> STRec(t, tyS_to_stype tS)
    | Language.STRecVar t -> STUVar(t)
    | Language.STDeclr _ -> assert false

let rec expF_to_exp expF =
    match expF with
    | Language.Int v -> Num v
    | Language.Bool v -> Bool v
    | Language.UOp(op, e) -> UOp(sessynth_uop_to_uop op, expF_to_exp e)
    | Language.BOp(Language.GrE, e1, e2) -> BOp(Or, BOp(Greater, expF_to_exp e1, expF_to_exp e2), BOp(Equals, expF_to_exp e1, expF_to_exp e2))
    | Language.BOp(Language.LtE, e1, e2) -> BOp(Or, BOp(Lesser, expF_to_exp e1, expF_to_exp e2), BOp(Equals, expF_to_exp e1, expF_to_exp e2))
    | Language.BOp(op, e1, e2) -> BOp(sessynth_bop_to_bop op, expF_to_exp e1, expF_to_exp e2)
    | Language.Var x -> Var x
    | Language.Let(x, e1, e2) -> Let(x, expF_to_exp e1, expF_to_exp e2)
    | Language.Lam(x, t, e) -> FunDef(x, Some (tyF_to_ty t), expF_to_exp e, None)
    | Language.App(e1, e2) -> FunApp(expF_to_exp e1, expF_to_exp e2)
    | Language.Ite(e1, e2, e3) -> Cond(expF_to_exp e1, expF_to_exp e2, expF_to_exp e3)
    | Language.Process(c, eP, _, csl) -> ProcExp(c, expP_to_proc eP, None, List.map(fun (c, s) -> (c, tyS_to_stype s)) csl)
    | Language.LetRec(_, _, e) -> expF_to_exp e

and expP_to_proc expP =
    match expP with
    | Language.SendF(c, eF, eP) -> Send(c, expF_to_exp eF, None, expP_to_proc eP)
    | Language.RecvF(c1, c2, eP) -> Recv(c1, c2, None, expP_to_proc eP)
    | Language.SendS(c1, c2, _, eP2) -> SendChan(c1, c2, None, expP_to_proc eP2)
    | Language.RecvS(c1, c2, eP) -> RecvChan(c1, c2, None, expP_to_proc eP)
    | Language.Close c -> Close c
    | Language.Wait(c, eP) -> Wait(c, expP_to_proc eP)
    | Language.Fwd(c1, c2, tS) -> Fwd(Some (tyS_to_stype tS), c2, c1)
    | Language.Choice(c, labelproclist) -> Choice(c, List.map(fun (l, p) -> (l, (expP_to_proc p, None))) labelproclist)
    | Language.ChoiceSelect(c, l, eP) -> Label(c, l, expP_to_proc eP, None)
    | Language.Spawn(c, eF, cl, eP) -> Spawn(c, expF_to_exp eF, None, expP_to_proc eP, cl)

(* adapter synth function *)

let synth p d goal =
    let p_sessynth = List.map(fun (x, t) -> (x, ty_to_tyF t)) p in
    let d_sessynth = List.map(fun (x, st) -> (x, stype_to_tyS st)) d in
    let synthed_expF = Sessynth.synth_ctxt p_sessynth d_sessynth goal in
    expF_to_exp synthed_expF