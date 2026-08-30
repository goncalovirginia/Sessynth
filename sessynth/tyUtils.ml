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

let offers_TProcess t =
    is_TProcess (get_return_type (match t with TForAll(_, t') -> t' | _ -> t))

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

(** The first name in [g] whose definition can reach itself, [None] when none can.
    Such a definition denotes no finite type -- recursion belongs in μ -- and both
    equivalence and unification resolve until they loop on one. *)
let cyclic_declr g =
    let rec names_tyS acc t =
        match t with
        | STDeclr x -> x::acc
        | STUnit | STRecVar _ -> acc
        | STSendF(f, t') | STRecvF(f, t') -> names_tyS (names_tyF acc f) t'
        | STSendS(t1, t2) | STRecvS(t1, t2) -> names_tyS (names_tyS acc t1) t2
        | STExtChoice l | STIntChoice l -> List.fold_left (fun a (_, t') -> names_tyS a t') acc l
        | STRec(_, _, t') -> names_tyS acc t'
    and names_tyF acc t =
        match t with
        | TAtomic _ | TRefinement _ | TDeclr _ -> acc
        | TArrow(t1, t2) -> names_tyF (names_tyF acc t1) t2
        | TForAll(_, t') -> names_tyF acc t'
        | TConstructor(_, args) -> List.fold_left names_tyF acc args
        | TProcess(incsl, outs) ->
            List.fold_left (fun a (_, t') -> names_tyS a t') (names_tyS acc outs) incsl
    in
    (* whether [x] is named anywhere [t] leads, [seen] stopping the walk at a name
       already followed so an unrelated cycle does not spin *)
    let rec reaches seen x t =
        List.exists (fun y ->
            y = x
            || (not (List.mem y seen)
                && match List.assoc_opt y g with
                   | Some t' -> reaches (y::seen) x t'
                   | None -> false))
            (names_tyS [] t)
    in
    List.find_map (fun (x, t) -> if reaches [x] x t then Some x else None) g

(** What Γ defines [x] to be. An undefined name is a malformed goal rather than
    a dead branch, hence the raise. *)
let lookup_declr g x =
    match List.assoc_opt x g with
    | Some t -> t
    | None -> raise (Fail ("session type " ^ x ^ " is not declared"))

(* equivalence *)

(** Whether [x1] and [x2] name the same binder under the pairing [env]: the one
    [env] pairs [x1] with, or -- when neither side binds it -- the same free name.
    The second half is what tells ∀x. a from ∀a. a. *)
let paired env x1 x2 =
    match List.assoc_opt x1 env with
    | Some x2' -> x2' = x2
    | None -> x1 = x2 && not (List.exists (fun (_, y) -> y = x2) env)

(** Equivalence of refinement predicates, where [x1] and [x2] are the binders the
    two refinements introduce. Any other name is free -- bound further up the arrow
    spine -- and has to be spelled the same. *)
let rec tyR_equiv x1 x2 r1 r2 =
    match r1, r2 with
    | RTInt v1, RTInt v2 -> v1 = v2
    | RTBool v1, RTBool v2 -> v1 = v2
    | RTVar y1, RTVar y2 -> if y1 = x1 || y2 = x2 then y1 = x1 && y2 = x2 else y1 = y2
    | RTUOp(o1, t1), RTUOp(o2, t2) -> o1 = o2 && tyR_equiv x1 x2 t1 t2
    | RTBOp(o1, a1, b1), RTBOp(o2, a2, b2) ->
        o1 = o2 && tyR_equiv x1 x2 a1 a2 && tyR_equiv x1 x2 b1 b2
    | _ -> false

(** Equivalence of session types; use this instead of (=) on any tyS.

    Modulo Γ, and modulo binders, each in its own way: a ∀'s names [env] pairs up
    as it descends (callers outside a scheme pass [[]]), a μ's are renamed apart,
    and a process type's input channel names are ignored outright. The unfolding
    budget of STRec is search bookkeeping, not meaning, so it is ignored too.

    Not modulo unfolding: μt.S and S[μt.S/t] stay distinct. Everything else is
    structural, refinement predicates included.

    NOTE: keep in sync with tyS/tyF whenever a constructor is added. *)
let rec tyS_equiv g env t1 t2 = tyS_equiv_rec g env [] t1 t2

(* [renv] pairs up the μ binders, as [env] does the ∀ ones. It is not merged with
   [env] because the two are separate namespaces that may collide on a name, and
   it needs no threading through tyF_equiv: no tyF mentions a recursion variable
   except inside a TProcess, which is a scope of its own. *)
and tyS_equiv_rec g env renv t1 t2 =
    match resolve_declr g t1, resolve_declr g t2 with
    | STUnit, STUnit -> true
    | STSendF(f1, s1), STSendF(f2, s2)
    | STRecvF(f1, s1), STRecvF(f2, s2) -> tyF_equiv g env f1 f2 && tyS_equiv_rec g env renv s1 s2
    | STSendS(a1, b1), STSendS(a2, b2)
    | STRecvS(a1, b1), STRecvS(a2, b2) ->
        tyS_equiv_rec g env renv a1 a2 && tyS_equiv_rec g env renv b1 b2
    | STExtChoice l1, STExtChoice l2
    | STIntChoice l1, STIntChoice l2 ->
        List.length l1 = List.length l2
        && List.for_all2 (fun (la, sa) (lb, sb) -> la = lb && tyS_equiv_rec g env renv sa sb) l1 l2
    | STRec(_, x1, s1), STRec(_, x2, s2) -> (* budget ignored, binders paired up *)
        tyS_equiv_rec g env ((x1, x2)::renv) s1 s2
    | STRecVar x1, STRecVar x2 -> paired renv x1 x2
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
       other side *)
    | TAtomic(TPolyVar a1), TAtomic(TPolyVar a2) -> paired env a1 a2
    | TAtomic a1, TAtomic a2 -> a1 = a2
    (* a refinement's own binder scopes over its predicate, so the two are compared
       up to it -- a rename of one side would capture a binder from further up the
       arrow spine, which {x:int | x>0} -> {y:int | y>x} has the predicate mention *)
    | TRefinement(x1, a1, r1), TRefinement(x2, a2, r2) ->
        a1 = a2 && tyR_equiv x1 x2 r1 r2
    | TArrow(a1, b1), TArrow(a2, b2) -> tyF_equiv g env a1 a2 && tyF_equiv g env b1 b2
    (* an input channel name is a binder -- the body reads it as Δ and the spawn
       site supplies an actual channel positionally -- so only the types count *)
    | TProcess(incsl1, outs1), TProcess(incsl2, outs2) ->
        List.length incsl1 = List.length incsl2
        && List.for_all2 (fun (_, s1) (_, s2) -> tyS_equiv g env s1 s2) incsl1 incsl2
        && tyS_equiv g env outs1 outs2
    | TDeclr x1, TDeclr x2 -> x1 = x2
    | TForAll(xl1, f1), TForAll(xl2, f2) ->
        (* the new pairs go in front, so an inner scheme shadows an outer one *)
        List.length xl1 = List.length xl2
        && tyF_equiv g (List.combine xl1 xl2 @ env) f1 f2
    | TConstructor(x1, args1), TConstructor(x2, args2) ->
        x1 = x2
        && List.length args1 = List.length args2
        && List.for_all2 (tyF_equiv g env) args1 args2
    | _ -> false
