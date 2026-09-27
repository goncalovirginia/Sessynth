open Syntax
open Sessynth

(* sessint -> sessynth *)

(* Everything sessynth can build but sessint cannot represent is thrown here *)
let unsupported what =
    raise (Sessynth.Fail ("cannot translate " ^ what ^ " between sessint and the synthesizer"))

let rec ty_to_tyF ty =
    match ty with
    | TUnit -> Language.TAtomic(TUnit)
    | TNum -> Language.TAtomic(TInt)
    | TBool -> Language.TAtomic(TBool)
    | TProc(st, l) -> Language.TProcess(List.map(fun (c, s) -> (c, stype_to_tyS s)) l, stype_to_tyS st)
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
  | STRec(v, st) -> Language.STRec(1, v, stype_to_tyS st)
  | STVar(x) -> Language.STRecVar(x)
  | STUVar(x) -> Language.STDeclr(x)
  | STMultiSend _ | STMultiRecv _ -> unsupported "a multisend or multireceive session type"

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
    | Language.GrE | Language.LtE -> unsupported "a >= or <= operator, which expF_to_exp expands before reaching here"

let tyA_to_ty tyA =
    match tyA with
    | Language.TInt -> TNum
    | Language.TBool -> TBool
    | Language.TUnit -> TUnit
    | Language.TPolyVar a | Language.TRigidVar a ->
        unsupported ("the type variable " ^ a ^ ", since sessint is not polymorphic")

(** The two type conversions back into sessint, closed over Γ. *)
let ty_converters g =
    let resolve = Sessynth.resolve_declr g in
    let rec tyF_to_ty tyF =
        match tyF with
        | Language.TAtomic tA -> tyA_to_ty tA
        | Language.TRefinement(_, tA, _) -> tyA_to_ty tA
        | Language.TArrow(tF1, tF2) -> TFun(tyF_to_ty tF1, tyF_to_ty tF2)
        | Language.TProcess(incsl, outs) -> TProc(tyS_to_stype outs, List.map(fun (c, s) -> (c, tyS_to_stype s)) incsl)
        | Language.TDeclr(x) -> TVar(x)
        | Language.TForAll _ -> unsupported "a polymorphic type scheme"
        | Language.TConstructor(_, x, _) -> unsupported ("the ADT " ^ x)

    and tyS_to_stype tyS =
        match resolve tyS with
        | Language.STSendF(tF, tS) -> STSend(tyF_to_ty tF, tyS_to_stype tS)
        | Language.STRecvF(tF, tS) -> STRecv(tyF_to_ty tF, tyS_to_stype tS)
        | Language.STSendS(tS1, tS2) -> STSendChan(tyS_to_stype tS1, tyS_to_stype tS2)
        | Language.STRecvS(tS1, tS2) -> STRecvChan(tyS_to_stype tS1, tyS_to_stype tS2)
        | Language.STUnit -> STEnd
        | Language.STExtChoice labelsesslist -> STExtChoice(List.map(fun (l, s) -> (l, tyS_to_stype s)) labelsesslist)
        | Language.STIntChoice labelsesslist -> STIntChoice(List.map(fun (l, s) -> (l, tyS_to_stype s)) labelsesslist)
        | Language.STRec(_, t, tS) -> STRec(t, tyS_to_stype tS)
        | Language.STRecVar t -> STVar(t)
        | Language.STDeclr x -> STUVar(x)
    in
    (tyF_to_ty, tyS_to_stype)

let union l1 l2 = l1 @ List.filter (fun x -> not (List.mem x l1)) l2
let without x l = List.filter (fun y -> y <> x) l

(** The channels [p] uses without binding them itself.

    Only the names are wanted: sessint's [Spawn] takes its context as a list of
    channel names and splits the caller's linear context by them (see
    [split_ctxt_for_spawn]), so the types come from there.

    Functional subterms are skipped. The only one that can mention a channel is
    a [Process], and its body sees exactly the inputs its own type declares, so
    it captures nothing from around it. *)
let rec free_channels_expP p =
    match p with
    | Language.Close c | Language.Hole(c, _) -> [c]
    | Language.Fwd(c1, c2, _) -> union [c1] [c2]
    | Language.Wait(c, p') | Language.ChoiceSelect(c, _, p')
    | Language.SendF(c, _, p') | Language.RecvF(_, _, c, p') ->
        union [c] (free_channels_expP p')
    | Language.SendS(c1, c2, p1, p2) ->
        union (union [c1] (without c2 (free_channels_expP p1))) (free_channels_expP p2)
    | Language.RecvS(c1, _, c2, p') -> union [c2] (without c1 (free_channels_expP p'))
    | Language.Choice(c, labelproclist) ->
        List.fold_left (fun acc (_, p') -> union acc (free_channels_expP p')) [c] labelproclist
    | Language.Spawn(c, _, cl, p') -> union cl (without c (free_channels_expP p'))

(** Whether [x] is named anywhere in [e]. Binders are not tracked because the
    only caller asks about a name the synthesizer made fresh, which nothing else
    rebinds, so a plain occurrence check is exact for it. *)
let rec mentions_expF x e =
    match e with
    | Language.Var y -> x = y
    | Language.Int _ | Language.Bool _ | Language.Unit -> false
    | Language.UOp(_, e1) | Language.Lam(_, _, e1) | Language.LetRec(_, _, e1) -> mentions_expF x e1
    | Language.BOp(_, e1, e2) | Language.Let(_, e1, e2) | Language.App(e1, e2) ->
        mentions_expF x e1 || mentions_expF x e2
    | Language.Ite(e1, e2, e3) ->
        mentions_expF x e1 || mentions_expF x e2 || mentions_expF x e3
    | Language.Process(_, p, _, _) -> mentions_expP x p
    | Language.Constructor(_, el) -> List.exists (mentions_expF x) el
    | Language.Match(e1, branches) ->
        mentions_expF x e1 || List.exists (fun (_, _, e2) -> mentions_expF x e2) branches

and mentions_expP x p =
    match p with
    | Language.Close _ | Language.Fwd _ | Language.Hole _ -> false
    | Language.RecvF(_, _, _, p') | Language.RecvS(_, _, _, p')
    | Language.Wait(_, p') | Language.ChoiceSelect(_, _, p') -> mentions_expP x p'
    | Language.SendF(_, e, p') -> mentions_expF x e || mentions_expP x p'
    | Language.SendS(_, _, p1, p2) -> mentions_expP x p1 || mentions_expP x p2
    | Language.Spawn(_, e, _, p') -> mentions_expF x e || mentions_expP x p'
    | Language.Choice(_, labelproclist) ->
        List.exists (fun (_, p') -> mentions_expP x p') labelproclist

(** Conversion of a synthesized term into sessint, closed over the names sessint
    already has in scope and over Γ. Only the [LetRec] case consults [declared]. *)
let expF_to_exp declared g =
    let (tyF_to_ty, tyS_to_stype) = ty_converters g in
    let rec expF_to_exp expF =
        match expF with
        | Language.Int v -> Num v
        | Language.Bool v -> Bool v
        | Language.Unit -> UnitVal
        | Language.UOp(op, e) -> UOp(sessynth_uop_to_uop op, expF_to_exp e)
        | Language.BOp(Language.GrE, e1, e2) -> BOp(Or, BOp(Greater, expF_to_exp e1, expF_to_exp e2), BOp(Equals, expF_to_exp e1, expF_to_exp e2))
        | Language.BOp(Language.LtE, e1, e2) -> BOp(Or, BOp(Lesser, expF_to_exp e1, expF_to_exp e2), BOp(Equals, expF_to_exp e1, expF_to_exp e2))
        | Language.BOp(op, e1, e2) -> BOp(sessynth_bop_to_bop op, expF_to_exp e1, expF_to_exp e2)
        | Language.Var x -> Var x
        | Language.Let(x, e1, e2) -> Let(x, expF_to_exp e1, expF_to_exp e2)
        | Language.Lam(x, t, e) -> FunDef(x, Some (tyF_to_ty t), expF_to_exp e, None)
        | Language.App(e1, e2) -> FunApp(expF_to_exp e1, expF_to_exp e2)
        | Language.Ite(e1, e2, e3) -> Cond(expF_to_exp e1, expF_to_exp e2, expF_to_exp e3)
        | Language.Process(c, eP, tS, csl) -> ProcExp(c, expP_to_proc eP, Some (tyS_to_stype tS), List.map(fun (c, s) -> (c, tyS_to_stype s)) csl)
        | Language.LetRec(x, _, e) ->
            (* sessint has no local recursive binding: a recursive function is a
               top-level declaration, in scope for its own body, and RecFunDef is
               only introduced by the preprocessor long after typechecking. When [x]
               names one of those declarations, the binder is redundant and drops
               away, which is the usual case; when the synthesizer invented the name
               the term needs something sessint cannot express, and saying so beats
               emitting a body whose recursive call refers to nothing. *)
            if mentions_expF x e && not (List.mem x declared) then
                raise (Sessynth.Fail ("synthesized a local recursive function " ^ x
                                      ^ ", which sessint cannot express: only top-level declarations may be recursive"))
            else expF_to_exp e
        | Language.Constructor(x, _) -> unsupported ("the constructor " ^ x)
        | Language.Match _ -> unsupported "a match expression"

    and expP_to_proc expP =
        match expP with
        | Language.SendF(c, eF, eP) -> Send(c, expF_to_exp eF, None, expP_to_proc eP)
        | Language.RecvF(x, tF, c, eP) -> Recv(x, c, Some (tyF_to_ty tF), expP_to_proc eP)
        | Language.SendS(c1, c2, eP1, eP2) ->
            (* sessint's SendChan only forwards a channel that is already in the
               linear context, whereas ⊗R bundles the offer with the send: [eP1] is
               the process that provides [c2]. The pair desugars into a spawn and
               then the send, naming the channels [eP1] consumes as the spawn's
               context. The annotations are left [None] and the spawned process's
               own context [[]] because the typechecker fills both in: it types the
               body against the context the spawn hands it. *)
            let args = without c2 (free_channels_expP eP1) in
            let offered = ProcExp(c2, expP_to_proc eP1, None, []) in
            Spawn(c2, offered, None, SendChan(c1, c2, None, expP_to_proc eP2), args)
        | Language.RecvS(c1, tS, c2, eP) -> RecvChan(c1, c2, Some (tyS_to_stype tS), expP_to_proc eP)
        | Language.Close c -> Close c
        | Language.Wait(c, eP) -> Wait(c, expP_to_proc eP)
        | Language.Fwd(c1, c2, tS) -> Fwd(Some (tyS_to_stype tS), c2, c1)
        | Language.Choice(c, labelproclist) -> Choice(c, List.map(fun (l, p) -> (l, (expP_to_proc p, None))) labelproclist)
        | Language.ChoiceSelect(c, l, eP) -> Label(c, l, expP_to_proc eP, None)
        | Language.Spawn(c, eF, cl, eP) -> Spawn(c, expF_to_exp eF, None, expP_to_proc eP, cl)
        | Language.Hole(c, _) -> raise (Sessynth.Fail ("unfilled synthesis placeholder on channel " ^ c))
    in
    expF_to_exp

(* adapter synth function *)

(** Splits sessint's one environment into the two the synthesizer keeps apart,
    on the case sessint's grammar already distinguishes: a term declaration is a
    lowercase VAR, a session type an uppercase S_VAR. [stype S ...] is recorded
    as a term binding anyway, purely so the name resolves later — but in Ψ that
    reads as a process to spawn, so it belongs in Γ. Type aliases fall out with
    it: a goal cannot name one, since TDeclr's production takes a lowercase VAR. *)
let functional_env_to_gamma_psi env =
    let names_a_term x = String.length x > 0 && x.[0] <> Char.uppercase_ascii x.[0] in
    let p = List.filter (fun (x, _) -> names_a_term x) env in
    let g = List.filter_map (fun (x, t) ->
        match t with
        | TProc(st, []) when not (names_a_term x) -> Some (x, stype_to_tyS st)
        | _ -> None) env
    in (g, p)

let synth nSolutions env d goal =
    let (g, p) = functional_env_to_gamma_psi env in
    let p_sessynth = List.map(fun (x, t) -> (x, ty_to_tyF t)) p in
    let d_sessynth = List.map(fun (x, st) -> (x, stype_to_tyS st)) d in
    let synthed_expF = Sessynth.synth nSolutions g p_sessynth [] d_sessynth goal in
    expF_to_exp (List.map fst p) g synthed_expF

(** The type a synthesized term is re-checked against: a goal naming a
    declaration resolves to what it declares, through Ψ for [TDeclr] and Γ for a
    session type, since everything else [check] sees is already expanded. *)
let goal_to_ty env goal =
    let (g, _) = functional_env_to_gamma_psi env in
    let (tyF_to_ty, _) = ty_converters g in
    match tyF_to_ty goal with
    | TVar(x) -> List.assoc x env
    | t -> t