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
    defect, [None] when it is not. Well formed is a scheme whose arrow spine ends
    at the datatype it builds, at its most general: [T ᾱ] over exactly the names
    the scheme binds, each once. Everything else defeats one of the two rules --
    a spine ending elsewhere raises inside the search, a name the result omits
    reaches the argument search undetermined, and a fixed parameter leaves the
    datatype's other constructors unable to unify with a scrutinee this one
    accepts, taking the whole match down rather than dropping a branch. *)
let ill_formed_constructor (x_c, t) =
    let clause d = Some ("the constructor " ^ x_c ^ " " ^ d) in
    match t with
    | TForAll(xl, t') ->
        let _, res = flatten_TArrow t' in
        begin
        match res with
        | TConstructor(_, _, args) ->
            (* the budget here is a placeholder, overwritten per instantiation;
               the arguments must be distinct names the scheme binds *)
            let rec general seen args =
                match args with
                | [] -> None
                | TAtomic(TPolyVar a)::args' ->
                    if not (List.mem a xl) then
                        clause ("returns the datatype at " ^ a ^ ", which it does not bind")
                    else if List.mem a seen then
                        clause ("returns the datatype at " ^ a ^ " twice")
                    else general (a::seen) args'
                | _::_ -> clause "returns the datatype at a type it fixes, rather than at a variable"
            in
            (match general [] args with
             | Some _ as d -> d
             | None ->
                 let determined = ftv_tyF res in
                 match List.find_opt (fun a -> not (S.mem a determined)) xl with
                 | Some a -> clause ("binds " ^ a ^ ", which its result type does not determine")
                 | None -> None)
        | _ -> clause "does not end at a datatype"
        end
    | _ -> clause "is not a type scheme"

(** The first datatype in [constructors] whose constructors disagree on how many
    parameters it takes, [None] when each is consistent. Both rules select a
    constructor by its datatype's name alone, so a disagreement would surface only
    as a branch that fails to unify. *)
let constructor_arity_disagreement constructors =
    let arity (_, t) =
        match t with
        | TForAll(_, t') ->
            (match snd (flatten_TArrow t') with
             | TConstructor(_, x, args) -> Some (x, List.length args)
             | _ -> None)
        | _ -> None
    in
    let rec check seen bindings =
        match bindings with
        | [] -> None
        | b::bindings' ->
            match arity b with
            | None -> check seen bindings'
            | Some (x, n) ->
                match List.assoc_opt x seen with
                | Some n' when n' <> n ->
                    Some ("the datatype " ^ x ^ " is declared with " ^ string_of_int n'
                          ^ " parameters by one constructor and " ^ string_of_int n ^ " by another")
                | _ -> check ((x, n)::seen) bindings'
    in check [] constructors

(* the constructors of the datatype that [c_T] is headed by, i.e. those bindings
   whose scheme returns the same datatype name *)
let constructors_of constructors c_T =
    match c_T with
    | TConstructor(_, goal_name, _) ->
        List.filter (fun (_, tF) ->
            match tF with
            | TForAll(_, tF') ->
                let _, res = flatten_TArrow tF' in
                begin
                match res with
                | TConstructor(_, res_name, _) -> res_name = goal_name
                | _ -> false
                end
            | _ -> raise (Fail "constructors_of: binding in constructors ctxt not a TForAll.")
        ) constructors
    | _ -> []

(** How the first datatype [t] names is at odds with [constructors], as a clause,
    [None] when every one lines up: nothing declares it, or it is used at an arity
    its constructors do not have. Either leaves both rules with no branch that can
    unify. Reads the arity off the first constructor, so run
    {!constructor_arity_disagreement} first. *)
let rec datatype_use_defect_tyF constructors t =
    let first_of tl =
        List.fold_left (fun acc t' ->
            match acc with Some _ -> acc | None -> datatype_use_defect_tyF constructors t') None tl
    in
    match t with
    | TAtomic _ | TRefinement _ | TDeclr _ -> None
    | TArrow(t1, t2) -> first_of [t1; t2]
    | TForAll(_, t') -> datatype_use_defect_tyF constructors t'
    | TConstructor(_, x, args) ->
        begin
        match constructors_of constructors t with
        | [] -> Some ("the datatype " ^ x ^ " has no constructors")
        | (_, tF)::_ ->
            let declared =
                match tF with
                | TForAll(_, tF') ->
                    (match snd (flatten_TArrow tF') with
                     | TConstructor(_, _, declared_args) -> List.length declared_args
                     | _ -> List.length args)
                | _ -> List.length args
            in
            if declared <> List.length args then
                Some ("the datatype " ^ x ^ " is used with " ^ string_of_int (List.length args)
                      ^ " parameters but declared with " ^ string_of_int declared)
            else first_of args
        end
    | TProcess(incsl, outs) ->
        List.fold_left (fun acc s ->
            match acc with Some _ -> acc | None -> datatype_use_defect_tyS constructors s)
            None (outs :: List.map snd incsl)

and datatype_use_defect_tyS constructors t =
    let first_of tl =
        List.fold_left (fun acc t' ->
            match acc with Some _ -> acc | None -> datatype_use_defect_tyS constructors t') None tl
    in
    match t with
    | STUnit | STRecVar _ | STDeclr _ -> None
    | STSendF(f, s) | STRecvF(f, s) ->
        (match datatype_use_defect_tyF constructors f with
         | Some _ as d -> d
         | None -> datatype_use_defect_tyS constructors s)
    | STSendS(s1, s2) | STRecvS(s1, s2) -> first_of [s1; s2]
    | STExtChoice l | STIntChoice l -> first_of (List.map snd l)
    | STRec(_, _, s) -> datatype_use_defect_tyS constructors s

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
        (* the scheme's own datatype occurrences sit one constructor below the
           target and spend one of its budget; those the substitution brings in
           are the target's type arguments and keep their own *)
        let args = List.map (set_budget_tyF (budget_of target - 1)) args in
        let args' = List.map (unify_subst_tyF subst) args in
        let ctxts' = unify_subst_ctxts subst ctxts in
        Some (f, ctxts', args', subst)
    with Fail _ -> None
