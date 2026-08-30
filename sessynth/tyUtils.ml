(* Operations on the synthesizer's types (tyF / tyS) and terms, independent of any
   context. Anything here may only mention Language; anything that needs a context
   belongs in Contexts instead. *)

open Language

exception Fail of string

(* projections and predicates *)

let rec get_return_type t =
    match t with
    | TArrow(_, t2) -> get_return_type t2
    | _ -> t

(** Whether left focus on [tFocus] could still end at [goal]. Only a decidable
    mismatch answers [false]; a [true] promises nothing, leaving the rule's own
    terminal comparison to decide. A refinement reads as its base type. *)
let return_type_may_match_goal tFocus goal =
    match get_return_type tFocus, goal with
    | (TAtomic a1 | TRefinement(_, a1, _)), (TAtomic a2 | TRefinement(_, a2, _)) -> a1 = a2
    | _ -> true

(* the name an application applies, under however many arguments it is given *)
let rec get_app_head_id e =
    match e with
    | Var x -> Some x
    | App(e1, _) -> get_app_head_id e1
    | _ -> None

let rec flatten_TArrow t =
    match t with
    | TArrow(t1, t2) ->
        let args, res = flatten_TArrow t2 in
        t1::args, res
    | _ -> [], t

let get_TProcess_insl t =
    match t with
    | TProcess(csl, _) -> Some (List.map (fun (_, s) -> s) csl)
    | _ -> None

let get_TProcess_outs t =
    match t with
    | TProcess(_, outs) -> Some outs
    | _ -> None

let is_TProcess t =
    match t with
    | TProcess _ -> true
    | _ -> false

let rec offers_TProcess t =
    match t with
    | TForAll(_, t') -> offers_TProcess t'
    | TArrow(_, t2) -> offers_TProcess t2
    | TProcess _ -> true
    | _ -> false

let is_STRec t =
    match t with
    | STRec _ -> true
    | _ -> false

let is_TRefinement t =
    match t with
    | TRefinement _ -> true
    | _ -> false


let is_tyF_left_async t =
    match t with
    | TConstructor _ -> true
    | _ -> false

let is_tyS_left_async t =
    match t with
    | STSendF _ | STSendS _ | STUnit | STIntChoice _ -> true
    | _ -> false

(* recursion *)

(* renames [from_id] to [to_id] throughout [t], stopping at a binder that would
   capture it *)
let rec rename_recvar_in_tyS from_id to_id t =
    match t with
    | STSendF(t1, t2) -> STSendF(t1, rename_recvar_in_tyS from_id to_id t2)
    | STRecvF(t1, t2) -> STRecvF(t1, rename_recvar_in_tyS from_id to_id t2)
    | STSendS(t1, t2) -> STSendS(rename_recvar_in_tyS from_id to_id t1, rename_recvar_in_tyS from_id to_id t2)
    | STRecvS(t1, t2) -> STRecvS(rename_recvar_in_tyS from_id to_id t1, rename_recvar_in_tyS from_id to_id t2)
    | STUnit -> STUnit
    | STExtChoice labelsesslist ->
        STExtChoice (List.map (fun (l, s) -> (l, rename_recvar_in_tyS from_id to_id s)) labelsesslist)
    | STIntChoice labelsesslist ->
        STIntChoice (List.map (fun (l, s) -> (l, rename_recvar_in_tyS from_id to_id s)) labelsesslist)
    | STRec(k, x, s) ->
        if x = from_id || x = to_id then STRec(k, x, s)
        else STRec(k, x, rename_recvar_in_tyS from_id to_id s)
    | STRecVar x -> if x = from_id then STRecVar to_id else t
    | STDeclr _ -> t

(* S[μt.S / t] *)
let rec unfold tUnfold stRec stRecVar =
    let unfold_both t1 t2 =
        match unfold t1 stRec stRecVar, unfold t2 stRec stRecVar with
        | Some t1', Some t2' -> Some (t1', t2')
        | _ -> None
    in
    let unfold_labels labelsesslist =
        List.fold_right (fun (l, t) acc ->
            match acc, unfold t stRec stRecVar with
            | Some acc', Some t' -> Some ((l, t')::acc')
            | _ -> None
        ) labelsesslist (Some [])
    in
    match tUnfold with
    | STSendF(t1, t2) ->
        Option.map (fun t2' -> STSendF(t1, t2')) (unfold t2 stRec stRecVar)
    | STRecvF(t1, t2) ->
        Option.map (fun t2' -> STRecvF(t1, t2')) (unfold t2 stRec stRecVar)
    | STSendS(t1, t2) ->
        Option.map (fun (t1', t2') -> STSendS(t1', t2')) (unfold_both t1 t2)
    | STRecvS(t1, t2) ->
        Option.map (fun (t1', t2') -> STRecvS(t1', t2')) (unfold_both t1 t2)
    | STUnit ->
        Some STUnit
    | STExtChoice labelsesslist ->
        Option.map (fun l -> STExtChoice l) (unfold_labels labelsesslist)
    | STIntChoice labelsesslist ->
        Option.map (fun l -> STIntChoice l) (unfold_labels labelsesslist)
    | STRec(k, x, t) ->
        if x = stRecVar then Some tUnfold (* rebound, so nothing below is ours *)
        else Option.map (fun t' -> STRec(k, x, t')) (unfold t stRec stRecVar)
    | STRecVar(x) ->
        if x = stRecVar then Some stRec (* where unfolding happens *)
        else Some tUnfold (* some other recursion variable *)
    | STDeclr _ -> Some tUnfold (* closed, so [stRecVar] cannot occur inside *)

(** The first recursion variable of [t] that no enclosing μ binds, [None] when
    [t] is closed. Nothing can unfold one away, so unchecked it surfaces as "no
    valid expression" rather than a malformed type. Session types nested in a
    process type are their own scopes, as in [unfold]. *)
let rec unbound_recvar_tyS bound t =
    let first_of tl = List.fold_left (fun acc t' ->
        match acc with Some _ -> acc | None -> unbound_recvar_tyS bound t') None tl
    in
    match t with
    | STUnit | STDeclr _ -> None
    | STSendF(f, t') | STRecvF(f, t') ->
        (match unbound_recvar_tyF f with Some x -> Some x | None -> unbound_recvar_tyS bound t')
    | STSendS(t1, t2) | STRecvS(t1, t2) -> first_of [t1; t2]
    | STExtChoice l | STIntChoice l -> first_of (List.map snd l)
    | STRec(_, x, t') -> unbound_recvar_tyS (x::bound) t'
    | STRecVar x -> if List.mem x bound then None else Some x

and unbound_recvar_tyF t =
    let first_of tl = List.fold_left (fun acc t' ->
        match acc with Some _ -> acc | None -> unbound_recvar_tyF t') None tl
    in
    match t with
    | TAtomic _ | TRefinement _ | TDeclr _ -> None
    | TArrow(t1, t2) -> first_of [t1; t2]
    | TForAll(_, t') -> unbound_recvar_tyF t'
    | TConstructor(_, args) -> first_of args
    | TProcess(incsl, outs) ->
        List.fold_left (fun acc t' ->
            match acc with Some _ -> acc | None -> unbound_recvar_tyS [] t')
            None (outs :: List.map snd incsl)

(** The refinement binders [t] introduces down its arrow spine, in order: the
    names the SyGuS encoding turns into symbols, each domain binder a parameter
    of the synthesized function and the codomain's its name. A domain that is not
    a refinement contributes nothing, its binder never reaching Ψ. *)
let rec refinement_binders t =
    match t with
    | TRefinement(x, _, _) -> [x]
    | TArrow(TRefinement(x, _, _), t2) -> x :: refinement_binders t2
    | TArrow(_, t2) -> refinement_binders t2
    | _ -> []

(** The head of [t] with any session-type declaration replaced by what Γ defines
    it to be, chasing a chain of them. A name Γ does not define is left alone --
    only {!lookup_declr} treats that as an error -- and one that leads back to
    itself stops rather than looping. *)
let resolve_declr g t =
    let rec resolve seen t =
        match t with
        | STDeclr x when not (List.mem x seen) ->
            (match List.assoc_opt x g with
             | Some t' -> resolve (x::seen) t'
             | None -> t)
        | _ -> t
    in resolve [] t

(** What Γ defines [x] to be. An undefined name is a malformed goal rather than
    a dead branch, hence the raise. *)
let lookup_declr g x =
    match List.assoc_opt x g with
    | Some t -> t
    | None -> raise (Fail ("session type " ^ x ^ " is not declared"))

(* equivalence *)

(** Equivalence of session types; use this instead of (=) on any tyS.

    Modulo Γ, and modulo the binders of STRec and TForAll, whose names [env]
    pairs up as it descends (callers outside a scheme pass [[]]). The unfolding
    budget of STRec is search bookkeeping, not meaning, so it is ignored.

    Not modulo unfolding: μt.S and S[μt.S/t] stay distinct. Everything else is
    structural, refinement predicates and TProcess input names included.

    NOTE: keep in sync with tyS/tyF whenever a constructor is added. *)
let rec tyS_equiv g env t1 t2 =
    match resolve_declr g t1, resolve_declr g t2 with
    | STUnit, STUnit -> true
    | STSendF(f1, s1), STSendF(f2, s2)
    | STRecvF(f1, s1), STRecvF(f2, s2) -> tyF_equiv g env f1 f2 && tyS_equiv g env s1 s2
    | STSendS(a1, b1), STSendS(a2, b2)
    | STRecvS(a1, b1), STRecvS(a2, b2) -> tyS_equiv g env a1 a2 && tyS_equiv g env b1 b2
    | STExtChoice l1, STExtChoice l2
    | STIntChoice l1, STIntChoice l2 ->
        List.length l1 = List.length l2
        && List.for_all2 (fun (la, sa) (lb, sb) -> la = lb && tyS_equiv g env sa sb) l1 l2
    | STRec(_, x1, s1), STRec(_, x2, s2) -> (* budget ignored, binder up to renaming *)
        let s2' = if x1 = x2 then s2 else rename_recvar_in_tyS x2 x1 s2 in
        tyS_equiv g env s1 s2'
    | STRecVar x1, STRecVar x2 -> x1 = x2
    | STDeclr x1, STDeclr x2 -> x1 = x2 (* only reached when Γ defines neither *)
    | _ -> false

(** Equivalence of functional types; see {!tyS_equiv}. Only needed because a
    functional type can carry session types (TProcess, and the value types
    exchanged by STSendF/STRecvF), so a stale (=) on a tyF hides the same
    unfolding-budget problem one level down. *)
and tyF_equiv g env t1 t2 =
    match t1, t2 with
    (* the one place [env] is read: a bound variable stands for the position its
       scheme bound it at, so it matches whatever sits at that position on the
       other side. A free one still has to be spelled the same -- and must not be
       bound on the other side, which is what tells ∀x. a from ∀a. a *)
    | TAtomic(TPolyVar a1), TAtomic(TPolyVar a2) ->
        (match List.assoc_opt a1 env with
         | Some a2' -> a2' = a2
         | None -> a1 = a2 && not (List.exists (fun (_, b) -> b = a2) env))
    | TAtomic a1, TAtomic a2 -> a1 = a2
    | TRefinement(x1, a1, r1), TRefinement(x2, a2, r2) -> x1 = x2 && a1 = a2 && r1 = r2
    | TArrow(a1, b1), TArrow(a2, b2) -> tyF_equiv g env a1 a2 && tyF_equiv g env b1 b2
    | TProcess(incsl1, outs1), TProcess(incsl2, outs2) ->
        List.length incsl1 = List.length incsl2
        && List.for_all2 (fun (c1, s1) (c2, s2) -> c1 = c2 && tyS_equiv g env s1 s2) incsl1 incsl2
        && tyS_equiv g env outs1 outs2
    | TDeclr x1, TDeclr x2 -> x1 = x2
    | TForAll(xkl1, f1), TForAll(xkl2, f2) ->
        (* the new pairs go in front, so an inner scheme shadows an outer one *)
        List.length xkl1 = List.length xkl2
        && List.for_all2 (fun (_, k1) (_, k2) -> k1 = k2) xkl1 xkl2
        && tyF_equiv g (List.map2 (fun (a1, _) (a2, _) -> (a1, a2)) xkl1 xkl2 @ env) f1 f2
    | TConstructor(x1, args1), TConstructor(x2, args2) ->
        x1 = x2
        && List.length args1 = List.length args2
        && List.for_all2 (tyF_equiv g env) args1 args2
    | _ -> false
