(* Algebraic datatypes.

   A constructor is recorded in the constructors context under the scheme

       x_c : ∀ᾱ. τ1 -> ... -> τn -> T ᾱ

   so that both ADT rules — building a value in right focus, and case-analysing
   one in left inversion — work by flattening that arrow spine and matching its
   result type against the type at hand. This module holds the knowledge of that
   shape; the rules themselves stay in Sessynth. *)

open Language
open TyUtils
open Contexts
open Polymorphism

(** How the constructor binding [(x_c, t)] is malformed, as a clause naming the
    defect, [None] when it is not.

    The shape both rules flatten is a scheme whose arrow spine ends at the
    datatype it builds; anything else reaches {!constructors_of} as a raise in
    the middle of the search, where nothing catches it.

    Every name the scheme binds has to occur in that result type, since
    {!instantiate_constructor} settles them by unifying it against the target. One
    the result leaves out is an existential -- [Foo : ∀a. a -> T] -- and would reach
    the argument search as a variable nothing determines. *)
let ill_formed_constructor (x_c, t) =
    let clause d = Some ("the constructor " ^ x_c ^ " " ^ d) in
    match t with
    | TForAll(xl, t') ->
        let _, res = flatten_TArrow t' in
        begin
        match res with
        | TConstructor _ ->
            let determined = ftv_tyF res in
            (match List.find_opt (fun a -> not (S.mem a determined)) xl with
             | Some a -> clause ("binds " ^ a ^ ", which its result type does not determine")
             | None -> None)
        | _ -> clause "does not end at a datatype"
        end
    | _ -> clause "is not a type scheme"

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

    [None] when the scheme's result type does not unify with [target].

    This is the boundary at which the exceptions raised by {!Polymorphism} are
    turned into a value: the whole body is eager, so catching here is sound,
    whereas a [try] placed around the monadic call site would not be — see
    {!ChoiceUtils.of_option}. *)
let instantiate_constructor f ctxts scheme target =
    try
        let f, ty_inst = instantiate_tyF f scheme in
        let args, res = flatten_TArrow ty_inst in
        let subst = unify ctxts.g res target in
        let args' = List.map (unify_subst_tyF subst) args in
        let ctxts' = unify_subst_ctxts subst ctxts in
        Some (f, ctxts', args', subst)
    with Fail _ -> None
