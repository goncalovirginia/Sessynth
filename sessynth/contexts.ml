(* The synthesizer's contexts and every operation that touches them. Operations
   on types alone belong in TyUtils; this module is what layers on top of it. *)

open Language
open TyUtils
open ChoiceUtils.Let_syntax

type bindingsS = (id * tyS) list
type bindingsF = (id * tyF) list
type constructors = bindingsF (* x_c : ∀ᾱ. τ1 -> ... -> τn -> T ᾱ *)

(* Each context is a single list. The left-asynchronous/synchronous split is a
   function of a binding's type alone (see TyUtils.is_tyF_left_async /
   is_tyS_left_async), so it is derived on demand instead of being materialized
   as two lists. *)
type contexts = {
    g : bindingsS;      (* Γ: unrestricted session-type declarations *)
    p : bindingsF;      (* Ψ: functional bindings *)
    c : constructors;   (* ADT constructors *)
    d : bindingsS;      (* Δ: linear session-typed channel bindings *)
}

let initialize_ctxts =
    let ctxts : contexts = { g = []; p = []; c = []; d = [] } in
    ctxts

(* extending a context *)

(* Extend a context with new bindings. Bindings are prepended, so the head of a
   context is its most recently introduced binding, and the order within
   [bindings] is preserved. *)
let append_bindings_gamma ctxts bindings = { ctxts with g = bindings @ ctxts.g }
let append_bindings_psi ctxts bindings = { ctxts with p = bindings @ ctxts.p }
let append_bindings_delta ctxts bindings = { ctxts with d = bindings @ ctxts.d }

(* selecting bindings *)

(* returns [Some (b, ctxt')], where [b] is the first binding of [ctxt] satisfying
   [predicate] and [ctxt'] is [ctxt] without that exact binding (the order of the
   remaining bindings is preserved), or [None] when no binding satisfies
   [predicate]. Removing the binding that was matched, rather than the first one
   carrying its name, avoids the shadowing hazard of List.remove_assoc when two
   bindings share a name. *)
let extract_first_binding predicate ctxt =
    let rec extract_first' seen rest =
        match rest with
        | [] -> None
        | b::rest' ->
            if predicate b then Some (b, List.rev_append seen rest')
            else extract_first' (b::seen) rest'
    in extract_first' [] ctxt

(* takes the next binding awaiting inversion *out* of a context: [Some ((x, t),
   ctxt')] with [(x, t)] no longer present in [ctxt'], or [None] once no
   left-asynchronous binding remains and the inversion phase is therefore
   finished. The inversion rules consume one binding at a time, hence the single
   binding plus remainder. *)
let extract_first_async is_left_async ctxt =
    extract_first_binding (fun (_, t) -> is_left_async t) ctxt

(* lists *all* the left-synchronous bindings of a context, and removes nothing
   from it. The decide rules need every candidate they may put in focus, and a
   failed focus backtracks into the next candidate over the unchanged context,
   hence a plain list rather than a binding plus remainder. *)
let get_sync_bindings is_left_async ctxt =
    List.filter (fun (_, t) -> not (is_left_async t)) ctxt

let find_binding_for_tyF ctxts tF =
    let (x, _) = List.find (fun (_, t) -> tyF_equiv t tF) ctxts.p in x

(* comparing contexts *)

let bindingS_equiv (x1, s1) (x2, s2) = x1 = x2 && tyS_equiv s1 s2

let bindingsS_equiv l1 l2 = List.equal bindingS_equiv l1 l2

let deltas_are_equal ctxtsl =
    let hdCtxts = List.hd ctxtsl in
    let are_equal = List.for_all (fun currCtxts ->
        bindingsS_equiv hdCtxts.d currCtxts.d
    ) (List.tl ctxtsl) in
    if are_equal then Some hdCtxts else None

let delta_is_empty ctxts =
    List.is_empty ctxts.d

(* consuming linear channels *)

let get_and_remove k kvl =
    let v = List.assoc k kvl in
    let kvl' = List.remove_assoc k kvl in
    kvl', v

let consume_channel ctxts c =
    let d', s = get_and_remove c ctxts.d in
    { ctxts with d = d' }, s

let rec consume_channels_by_tyS ctxts tSl =
    match tSl with
    | [] -> Choice.return (ctxts, [])
    | tS::tSl' ->
        let csl = List.filter (fun (_, s) -> tyS_equiv s tS) ctxts.d in
        let* (c, _) = Choice.of_list csl in
        let ctxts', _ = consume_channel ctxts c in
        let* (ctxts'', cl) = consume_channels_by_tyS ctxts' tSl' in
        Choice.return (ctxts'', c::cl)

