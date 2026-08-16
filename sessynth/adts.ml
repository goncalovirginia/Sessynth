(* Algebraic datatypes.

   A constructor is recorded in the constructors context under the scheme

       x_c : ∀ᾱ. τ1 -> ... -> τn -> T ᾱ

   so that both ADT rules — building a value in right focus, and case-analysing
   one in left inversion — work by flattening that arrow spine and matching its
   result type against the type at hand. This module holds the knowledge of that
   shape; the rules themselves stay in Sessynth. *)

open Language
open TyUtils
open Polymorphism

(* the constructors of the datatype that [c_T] is headed by, i.e. those bindings
   whose scheme returns the same datatype name *)
let constructors_of constructors c_T =
    match c_T with
    | TConstructor(goal_name, _) ->
        List.filter (fun (_, tF) ->
            match tF with
            | TForAll(_, tF') ->
                let _, res = flatten_TArrow tF' in
                begin
                match res with
                | TConstructor(res_name, _) -> res_name = goal_name
                | _ -> false
                end
            | _ -> raise (Fail "constructors_of: binding in constructors ctxt not a TForAll.")
        ) constructors
    | _ -> []

(** Instantiate the constructor scheme [scheme] with fresh type variables and
    match its result type against [target].

    Returns the advanced flags, the contexts with the unifying substitution
    applied, the constructor's argument types τ1..τn under that substitution,
    and the substitution itself — the caller still needs it to rewrite whatever
    else it is carrying (the goal, in the case-analysis rule).

    Raises {!TyUtils.Fail} when the scheme's result type does not unify with
    [target]. *)
let instantiate_constructor f ctxts scheme target =
    let f, ty_inst = instantiate_tyF f scheme in
    let args, res = flatten_TArrow ty_inst in
    let subst = unify res target in
    let args' = List.map (unify_subst_tyF subst) args in
    let ctxts' = unify_subst_ctxts subst ctxts in
    f, ctxts', args', subst
