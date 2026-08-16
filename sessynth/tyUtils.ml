(* Operations on the synthesizer's types (tyF / tyS), independent of any context.
   Anything here may only mention Language; anything that needs a context belongs
   in Contexts instead. *)

open Language

exception Fail of string

(* projections and predicates *)

let rec get_return_type t =
    match t with
    | TArrow(t1, t2) -> get_return_type t2
    | _ -> t

let rec flatten_TArrow t =
    match t with
    | TArrow(t1, t2) ->
        let args, res = flatten_TArrow t2 in
        t1::args, res
    | _ -> [], t

let get_TProcess_insl t =
    match t with
    | TProcess(csl, _) -> Some (List.map (fun (c, s) -> s) csl)
    | _ -> None

let get_TProcess_outs t =
    match t with
    | TProcess(_, outs) -> Some outs
    | _ -> None

let is_TProcess t =
    match t with
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

(* A binding is left-asynchronous (invertible on the left) purely as a function
   of its type, which is why the async/sync split of a context is derived on
   demand rather than materialized. *)

let is_tyF_left_async t =
    match t with
    | TConstructor _ -> true
    | _ -> false

let is_tyS_left_async t =
    match t with
    | STSendF _ | STSendS _ | STUnit | STIntChoice _ -> true
    | _ -> false

(* recursion *)

(* renames the recursion variable [from_id] to [to_id] throughout [t], stopping
   at any binder that would capture it *)
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

(* S[μt.S / t]; None when an unresolved session-type declaration is reached,
   since it cannot be unfolded without Γ *)
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
        if x = stRecVar then Some tUnfold (* prevents infinite unfolding if for some reason μt.S is used inside the original μt.S *)
        else Option.map (fun t' -> STRec(k, x, t')) (unfold t stRec stRecVar) (* in case another recursive session type μt2.S2 is used inside μt.S *)
    | STRecVar(x) ->
        if x = stRecVar then Some stRec (* where unfolding happens *)
        else Some tUnfold (* if it's another recursive name t2 != t, simply return t2 *)
    | STDeclr _ -> None

(* equivalence *)

(** Equivalence of session types.

    Use this instead of (=) on any tyS. The unfolding budget [k] of STRec is
    search bookkeeping rather than part of the type's meaning, so structural
    equality would wrongly distinguish μ¹t.S from μ⁰t.S — which matters wherever
    a type is compared after it has been partially unfolded. The recursion
    variable of STRec is a binder, so it is compared up to renaming.

    Everything else is compared structurally, exactly as (=) would: in
    particular refinement predicates, TForAll binders and the input channel
    names of TProcess are all still significant.

    This is not equivalence modulo unfolding: μt.S and S[μt.S/t] are still
    distinguished.

    NOTE: keep in sync with tyS/tyF whenever a constructor is added. *)
let rec tyS_equiv t1 t2 =
    match t1, t2 with
    | STUnit, STUnit -> true
    | STSendF(f1, s1), STSendF(f2, s2)
    | STRecvF(f1, s1), STRecvF(f2, s2) -> tyF_equiv f1 f2 && tyS_equiv s1 s2
    | STSendS(a1, b1), STSendS(a2, b2)
    | STRecvS(a1, b1), STRecvS(a2, b2) -> tyS_equiv a1 a2 && tyS_equiv b1 b2
    | STExtChoice l1, STExtChoice l2
    | STIntChoice l1, STIntChoice l2 ->
        List.length l1 = List.length l2
        && List.for_all2 (fun (la, sa) (lb, sb) -> la = lb && tyS_equiv sa sb) l1 l2
    | STRec(_, x1, s1), STRec(_, x2, s2) -> (* budget ignored, binder up to renaming *)
        let s2' = if x1 = x2 then s2 else rename_recvar_in_tyS x2 x1 s2 in
        tyS_equiv s1 s2'
    | STRecVar x1, STRecVar x2 -> x1 = x2
    | STDeclr x1, STDeclr x2 -> x1 = x2
    | _ -> false

(** Equivalence of functional types; see {!tyS_equiv}. Only needed because a
    functional type can carry session types (TProcess, and the value types
    exchanged by STSendF/STRecvF), so a stale (=) on a tyF hides the same
    unfolding-budget problem one level down. *)
and tyF_equiv t1 t2 =
    match t1, t2 with
    | TAtomic a1, TAtomic a2 -> a1 = a2
    | TRefinement(x1, a1, r1), TRefinement(x2, a2, r2) -> x1 = x2 && a1 = a2 && r1 = r2
    | TArrow(a1, b1), TArrow(a2, b2) -> tyF_equiv a1 a2 && tyF_equiv b1 b2
    | TProcess(incsl1, outs1), TProcess(incsl2, outs2) ->
        List.length incsl1 = List.length incsl2
        && List.for_all2 (fun (c1, s1) (c2, s2) -> c1 = c2 && tyS_equiv s1 s2) incsl1 incsl2
        && tyS_equiv outs1 outs2
    | TDeclr x1, TDeclr x2 -> x1 = x2
    | TForAll(xkl1, f1), TForAll(xkl2, f2) -> xkl1 = xkl2 && tyF_equiv f1 f2
    | TConstructor(x1, args1), TConstructor(x2, args2) ->
        x1 = x2
        && List.length args1 = List.length args2
        && List.for_all2 tyF_equiv args1 args2
    | _ -> false
