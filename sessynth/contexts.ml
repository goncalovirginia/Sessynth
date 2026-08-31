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
    xRecLam : id;       (* the name Ψ binds the current enclosing recursive lambda to, "" outside one *)
    released : tyF list;(* the datatypes left focus has released, bounding further releases *)
}

let initialize_ctxts =
    let ctxts : contexts = { g = []; p = []; c = []; d = []; xRecLam = ""; released = [] } in
    ctxts

(* extending a context *)

(* Extend a context with new bindings. Bindings are prepended, so the head of a
   context is its most recently introduced binding, and the order within
   [bindings] is preserved. *)
let append_bindings_gamma ctxts bindings = { ctxts with g = bindings @ ctxts.g }
let append_bindings_psi ctxts bindings = { ctxts with p = bindings @ ctxts.p }
let append_bindings_constructors ctxts bindings = { ctxts with c = bindings @ ctxts.c }
let append_bindings_delta ctxts bindings = { ctxts with d = bindings @ ctxts.d }

(* handing a context back *)

(** The contexts a rule returns: [outer] is what it was given, [inner] is what its
    subderivation produced. Δ is linear, so what it left unconsumed carries on;
    every other field is an input, and what a rule adds to one belongs to its own
    subterm, not to whatever the caller synthesizes next. *)
let restore_scope outer inner = { outer with d = inner.d }

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
    Option.map fst (List.find_opt (fun (_, t) -> tyF_equiv ctxts.g [] t tF) ctxts.p)

(* comparing contexts *)

let bindingS_equiv ctxts (x1, s1) (x2, s2) = x1 = x2 && tyS_equiv ctxts.g [] s1 s2

(** Multiset equality under [bindingS_equiv].

   Δ is a linear context, so the order its bindings happen to sit in carries no
   meaning: two branches that consumed the same channels in a different sequence
   are left holding the same resources.

   Each binding in [l1] is matched against a distinct partner in [l2] rather than
   the two lists being compacted into sets, so repeated bindings stay significant:
   [a:S, b:S] and [a:S, a:S, b:S] are still different contexts. *)
let bindingsS_equiv ctxts l1 l2 =
    let rec remove_first b skipped l =
        match l with
        | [] -> None
        | b'::l' ->
            if bindingS_equiv ctxts b b' then Some (List.rev_append skipped l')
            else remove_first b (b'::skipped) l'
    in
    let rec match_up l1 l2 =
        match l1 with
        | [] -> List.is_empty l2
        | b::l1' ->
            match remove_first b [] l2 with
            | None -> false
            | Some l2' -> match_up l1' l2'
    in
    List.compare_lengths l1 l2 = 0 && match_up l1 l2

(** Returns [Some ctxts] when every branch left Δ holding the same bindings, [None]
   otherwise. The contexts of the first branch are the ones carried forward;
   though any of them would do, since they differ at most in the order of Δ.
   An empty list yields [None]: there is no branch context to carry forward, so
   the caller has nothing to continue with even though the condition is
   vacuously true. 
   Only the returned Δ is meaningful, so the caller should pass the return value
   through {!restore_scope} when used like other subderivations. *)
let deltas_are_equal ctxtsl =
    match ctxtsl with
    | [] -> None
    | hdCtxts::tlCtxts ->
        let are_equal = List.for_all (fun currCtxts ->
            bindingsS_equiv hdCtxts hdCtxts.d currCtxts.d
        ) tlCtxts in
        if are_equal then Some hdCtxts else None

let delta_is_empty ctxts =
    List.is_empty ctxts.d

(** Returns [Some x] for the first name bound twice in [bindings], [None] when every
   name is distinct. *)
let find_first_duplicate_name bindings =
    let rec find seen bindings =
        match bindings with
        | [] -> None
        | (x, _)::bindings' ->
            if List.mem x seen then Some x else find (x::seen) bindings'
    in find [] bindings

(* consuming linear channels *)

(* removes channel [c] from Δ, returning the shrunk contexts and the session
   type [c] was carrying; None when [c] is not in Δ (already consumed, or never
   there) *)
let consume_channel ctxts c =
    match List.assoc_opt c ctxts.d with
    | None -> None
    | Some s -> Some ({ ctxts with d = List.remove_assoc c ctxts.d }, s)

let rec consume_channels_by_tyS ctxts tSl =
    match tSl with
    | [] -> Choice.return (ctxts, [])
    | tS::tSl' ->
        let csl = List.filter (fun (_, s) -> tyS_equiv ctxts.g [] s tS) ctxts.d in
        let* (c, _) = Choice.of_list csl in
        let* (ctxts', _) = ChoiceUtils.of_option (consume_channel ctxts c) in
        let* (ctxts'', cl) = consume_channels_by_tyS ctxts' tSl' in
        Choice.return (ctxts'', c::cl)

