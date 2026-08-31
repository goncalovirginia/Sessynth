(* Parametric polymorphism: free type variables, opening a scheme flexibly or
   rigidly, and first-order unification.

   Rank-N and predicative -- a ∀ may sit wherever a type may, but no variable
   stands for one. A variable the goal determines is unrestricted; one it leaves
   undetermined is guessed from the ground types instead, so that one can only be
   int or bool. Higher kinds are absent by construction: TPolyVar is a tyA, so
   there is nowhere to write the type constructor they would quantify over. *)

open Language
open TyUtils
open Contexts
open Flags

module S = Set.Make(String)

(* free type variables *)

(** a rigid variable stands for a type already chosen, so it is as much a constant
   here as an int is: not free, never guessed by [!ground_substitutions] *)
let ftv_tyA = function
    | TInt | TBool | TRigidVar _ -> S.empty
    | TPolyVar a -> S.singleton a

let rec ftv_tyR = function
    | RTInt _ | RTBool _ | RTVar _ -> S.empty
    | RTUOp (_, r) -> ftv_tyR r
    | RTBOp (_, r1, r2) -> S.union (ftv_tyR r1) (ftv_tyR r2)

let rec ftv_tyF = function
    | TAtomic a -> ftv_tyA a
    | TRefinement(_, a, r) -> S.union (ftv_tyA a) (ftv_tyR r)
    | TArrow(t1, t2) -> S.union (ftv_tyF t1) (ftv_tyF t2)
    | TProcess(incsl, outs) ->
        List.fold_left (fun s (_, st) -> S.union s (ftv_tyS st)) (ftv_tyS outs) incsl
    | TDeclr _ -> S.empty
    | TForAll(xl, t) -> S.diff (ftv_tyF t) (S.of_list xl)
    | TConstructor(_, _, args) ->
        List.fold_left (fun s t -> S.union s (ftv_tyF t)) S.empty args

(* a recursion variable is not a type variable, so only the functional types a
   session type carries can contribute one. {!occurs_tyS} already descended here;
   this is the half that did not. *)
and ftv_tyS = function
    | STUnit | STRecVar _ | STDeclr _ -> S.empty
    | STSendF(t, s) | STRecvF(t, s) -> S.union (ftv_tyF t) (ftv_tyS s)
    | STSendS(s1, s2) | STRecvS(s1, s2) -> S.union (ftv_tyS s1) (ftv_tyS s2)
    | STExtChoice l | STIntChoice l ->
        List.fold_left (fun s (_, st) -> S.union s (ftv_tyS st)) S.empty l
    | STRec(_, _, s) -> ftv_tyS s

(** The first type variable of [t] that no ∀ binds, [None] when it is closed.
    Nothing determines such a variable, so unchecked it reaches the rules as a
    goal they cannot answer. *)
let unbound_polyvar_tyF t = S.min_elt_opt (ftv_tyF t)
let unbound_polyvar_tyS t = S.min_elt_opt (ftv_tyS t)

(** How [t] is malformed as far as ∀ is concerned, as a clause naming the offender,
    [None] when it is not.

    A ∀ binding one name twice reads two ways -- [!open_scheme] substitutes only the
    last repeat, [!TyUtils.tyF_equiv] pairs up the first -- and a rigid variable is
    one only [!skolemize_tyF] makes, standing for a type whoever calls has chosen,
    so in input there is no caller to have chosen it. Neither reaches here from a
    .sessint file, whose grammar has no schemes; both reach here from the API. *)
let rec ill_formed_scheme_tyF t =
    let first_of tl =
        List.fold_left (fun acc t' ->
            match acc with Some _ -> acc | None -> ill_formed_scheme_tyF t') None tl
    in
    match t with
    | TAtomic(TRigidVar r) | TRefinement(_, TRigidVar r, _) ->
        Some ("the internal rigid variable " ^ r)
    | TAtomic _ | TRefinement _ | TDeclr _ -> None
    | TArrow(t1, t2) -> first_of [t1; t2]
    | TConstructor(_, _, args) -> first_of args
    | TProcess(incsl, outs) ->
        List.fold_left (fun acc s ->
            match acc with Some _ -> acc | None -> ill_formed_scheme_tyS s)
            None (outs :: List.map snd incsl)
    | TForAll(xl, t') ->
        (* substitution protects a rebound name but not the type substituted in, so
           a binder spelled like a fresh variable captures it: opening
           ∀a. (∀_α0. _α0 -> a) -> int at _α0 turns the argument into the identity.
           Reserving the prefix is what makes the convention an invariant *)
        let reserved a = String.length a > 0 && a.[0] = '_' in
        let rec offender seen xs =
            match xs with
            | [] -> None
            | a::xs' ->
                if reserved a then Some ("a forall binding the reserved name " ^ a)
                else if List.mem a seen then Some ("a forall binding " ^ a ^ " twice")
                else offender (a::seen) xs'
        in
        (match offender [] xl with
         | Some _ as d -> d
         | None -> ill_formed_scheme_tyF t')

and ill_formed_scheme_tyS t =
    let first_of tl =
        List.fold_left (fun acc t' ->
            match acc with Some _ -> acc | None -> ill_formed_scheme_tyS t') None tl
    in
    match t with
    | STUnit | STRecVar _ | STDeclr _ -> None
    | STSendF(f, s) | STRecvF(f, s) ->
        (match ill_formed_scheme_tyF f with
         | Some _ as d -> d
         | None -> ill_formed_scheme_tyS s)
    | STSendS(s1, s2) | STRecvS(s1, s2) -> first_of [s1; s2]
    | STExtChoice l | STIntChoice l -> first_of (List.map snd l)
    | STRec(_, _, s) -> ill_formed_scheme_tyS s

(* the types a variable may be guessed at when unification doesn't determine it *)
let ground_atomic_types = [TInt; TBool]

(** Returns every combination of ground type substitutions for each free variable of [t] (i.e. a list of substitution lists). 
    A [t] that has none returns a list containing a single empty substitution list, leaving a caller that never had a variable exactly as it was. *)
let ground_substitutions t =
    List.fold_left (fun substs a ->
        List.concat_map (fun s ->
            List.map (fun tA -> (a, TAtomic tA)::s) ground_atomic_types
        ) substs
    ) [[]] (S.elements (ftv_tyF t))

(* instantiation *)

let instantiate_subst_tyA subst t =
    match t with
    | TInt | TBool | TRigidVar _ -> t
    | TPolyVar a -> try List.assoc a subst with Not_found -> TPolyVar a

let rec instantiate_subst_tyF subst t =
    match t with
    | TAtomic a -> TAtomic (instantiate_subst_tyA subst a)
    | TRefinement (x, a, r) -> TRefinement (x, instantiate_subst_tyA subst a, r)
    | TArrow (t1, t2) -> TArrow (instantiate_subst_tyF subst t1, instantiate_subst_tyF subst t2)
    | TProcess (incsl, outs) -> TProcess (List.map (fun (c, s) -> (c, instantiate_subst_tyS subst s)) incsl, instantiate_subst_tyS subst outs)
    | TDeclr x -> TDeclr x
    | TForAll (xl, tF) ->
        (* avoid capture: ignore substitution for re-bound vars *)
        let subst' = List.filter (fun (a, _) -> not (List.mem a xl)) subst in
        TForAll (xl, instantiate_subst_tyF subst' tF)
    | TConstructor(k, x, args) ->
        TConstructor(k, x, List.map (fun arg -> instantiate_subst_tyF subst arg) args)

and instantiate_subst_tyS subst t =
    match t with
    | STSendF(t1, t2) -> STSendF(instantiate_subst_tyF subst t1, instantiate_subst_tyS subst t2)
    | STRecvF(t1, t2) -> STRecvF(instantiate_subst_tyF subst t1, instantiate_subst_tyS subst t2)
    | STSendS(t1, t2) -> STSendS(instantiate_subst_tyS subst t1, instantiate_subst_tyS subst t2)
    | STRecvS(t1, t2) -> STRecvS(instantiate_subst_tyS subst t1, instantiate_subst_tyS subst t2)
    | STUnit -> STUnit
    | STExtChoice labelsesslist ->
        STExtChoice (List.map (fun (l, s) -> (l, instantiate_subst_tyS subst s)) labelsesslist)
    | STIntChoice labelsesslist ->
        STIntChoice (List.map (fun (l, s) -> (l, instantiate_subst_tyS subst s)) labelsesslist)
    | STRec(k, x, s) -> STRec(k, x, instantiate_subst_tyS subst s)
    | STRecVar _ -> t
    | STDeclr _ -> t

(** ∀ᾱ. F → F[β̄/ᾱ], with [fresh_var] deciding which sort of variable the bound
   names are replaced by *)
let open_scheme fresh_var f t =
    match t with
    | TForAll (xl, tF) ->
        let f, subst =
            List.fold_left (fun (f', subst') a ->
                let f'', v = fresh_var f' in
                f'', (a, v)::subst'
            ) (f, []) xl
        in
        f, instantiate_subst_tyF subst tF
    | _ -> f, t

(** Opens a scheme being *used*: the variables are holes this derivation may fill,
    since it is the one choosing what the scheme is applied at. *)
let instantiate_tyF f t =
    open_scheme (fun f -> let f, k = fresh_kind f in f, TPolyVar k) f t

(** Opens a scheme being *offered*: the variables stand for types whoever calls
    has already chosen, so the body has to work without knowing them and nothing
    here may bind or guess one. *)
let skolemize_tyF f t =
    open_scheme (fun f -> let f, r = fresh_rigid f in f, TRigidVar r) f t

(* applying a unifying substitution *)

let unify_subst_tyA s a =
    match a with
    | TInt | TBool | TRigidVar _ -> a
    | TPolyVar v ->
        match List.assoc_opt v s with
        | Some (TAtomic a') -> a'
        | Some (TRefinement(_, a', _)) -> a'
        | Some _ -> raise (Fail "unify_subst: a refinement's base type can only be a base type")
        | None -> a

let rec unify_subst_tyF s t =
    match t with
    | TAtomic(TPolyVar a) -> begin
        match List.assoc_opt a s with
        | Some t' -> t'
        | None -> t
        end
    | TAtomic _ -> t
    | TArrow(t1, t2) -> TArrow (unify_subst_tyF s t1, unify_subst_tyF s t2)
    | TRefinement(x, a, r) -> TRefinement (x, unify_subst_tyA s a, r)
    | TProcess(cs, s') -> TProcess (List.map (fun (c, st) -> (c, unify_subst_tyS s st)) cs, unify_subst_tyS s s')
    | TDeclr(x) -> TDeclr x
    | TForAll(xl, t') ->
        let s' = List.filter (fun (a, _) -> not (List.mem a xl)) s in
        TForAll (xl, unify_subst_tyF s' t')
    | TConstructor(k, x, args) -> TConstructor(k, x, List.map (fun arg -> unify_subst_tyF s arg) args)

and unify_subst_tyS s t =
    match t with
    | STSendF(t1, t2) -> STSendF(unify_subst_tyF s t1, unify_subst_tyS s t2)
    | STRecvF(t1, t2) -> STRecvF(unify_subst_tyF s t1, unify_subst_tyS s t2)
    | STSendS(t1, t2) -> STSendS(unify_subst_tyS s t1, unify_subst_tyS s t2)
    | STRecvS(t1, t2) -> STRecvS(unify_subst_tyS s t1, unify_subst_tyS s t2)
    | STUnit -> STUnit
    | STExtChoice labelsesslist ->
        STExtChoice (List.map (fun (l, st) -> (l, unify_subst_tyS s st)) labelsesslist)
    | STIntChoice labelsesslist ->
        STIntChoice (List.map (fun (l, st) -> (l, unify_subst_tyS s st)) labelsesslist)
    | STRec(k, x, st) -> STRec(k, x, unify_subst_tyS s st)
    | STRecVar _ -> t
    | STDeclr _ -> t

let unify_subst_ctxts subst ctxts =
    {
        ctxts with
        g = List.map (fun (x, st) -> (x, unify_subst_tyS subst st)) ctxts.g;
        p = List.map (fun (x, t) -> (x, unify_subst_tyF subst t)) ctxts.p;
        c = List.map (fun (x, t) -> (x, unify_subst_tyF subst t)) ctxts.c;
        d = List.map (fun (x, st) -> (x, unify_subst_tyS subst st)) ctxts.d;
    }

(* apply substitution to entire substitution set *)
let unify_subst_subst s1 s2 =
    List.map (fun (x, t) -> (x, unify_subst_tyF s1 t)) s2

(* apply s2, then s1 *)
let unify_compose_subst s1 s2 =
    s1 @ unify_subst_subst s1 s2

(* unification *)

(* check to avoid α = α -> α *)
let rec occurs a t =
    match t with
    | TAtomic(TPolyVar b) -> a = b
    | TAtomic _ -> false
    | TArrow(t1, t2) -> occurs a t1 || occurs a t2
    | TRefinement(_, tA, _) -> tA = TPolyVar a
    | TProcess(cs, s') ->
        List.exists (fun (_, st) -> occurs_tyS a st) cs || occurs_tyS a s'
    | TDeclr _ -> false
    | TForAll(xl, t') -> if List.mem a xl then false else occurs a t'
    | TConstructor(_, _, args) -> List.exists (fun arg -> occurs a arg) args

and occurs_tyS a t =
    match t with
    | STSendF(t1, t2) | STRecvF(t1, t2) -> occurs a t1 || occurs_tyS a t2
    | STSendS(t1, t2) | STRecvS(t1, t2) -> occurs_tyS a t1 || occurs_tyS a t2
    | STUnit -> false
    | STExtChoice labelsesslist | STIntChoice labelsesslist ->
        List.exists (fun (_, st) -> occurs_tyS a st) labelsesslist
    | STRec(_, _, st) -> occurs_tyS a st
    | STRecVar _ -> false
    | STDeclr _ -> false

(* unify(t1​, t2​) = θ *)
let rec unify g t1 t2 =
    match t1, t2 with
    | TAtomic(TInt), TAtomic(TInt) -> []
    | TAtomic(TBool), TAtomic(TBool) -> []
    (* two rigids unify only with themselves: each stands for a type someone else
       chose, and nothing here knows whether two such choices agree *)
    | TAtomic(TRigidVar(x)), TAtomic(TRigidVar(y)) -> if x = y then [] else raise (Fail "Cannot unify.")
    | TAtomic(TPolyVar(a)), t | t, TAtomic(TPolyVar(a)) ->
        let t' =
            match t with
            | TRefinement(_, a2, _) -> TAtomic a2
            | _ -> t
        in
        begin
        match t' with
        | TForAll _ -> raise (Fail "Cannot unify: a type variable may not stand for a scheme")
        | _ ->
            if t' = TAtomic(TPolyVar(a)) then []
            else if occurs a t' then raise (Fail "Occurs check failed.")
            else [(a, t')]
        end
    | TRefinement(_, a1, _), TRefinement(_, a2, _) -> unify g (TAtomic a1) (TAtomic a2)
    | TRefinement(_, a, _), t | t, TRefinement(_, a, _) -> unify g (TAtomic a) t
    | TArrow(a1, b1), TArrow(a2, b2) ->
        let s1 = unify g a1 a2 in
        let s2 = unify g (unify_subst_tyF s1 b1) (unify_subst_tyF s1 b2) in
        unify_compose_subst s2 s1
    | TProcess(cs1, outs1), TProcess(cs2, outs2) ->
        if List.length cs1 <> List.length cs2 then raise (Fail "Cannot unify.")
        else
            let subst0 = unify_tyS g outs1 outs2 in
            List.fold_left2
                (fun subst_acc (_, s1) (_, s2) ->
                    let s1' = unify_subst_tyS subst_acc s1 in
                    let s2' = unify_subst_tyS subst_acc s2 in
                    unify_compose_subst (unify_tyS g s1' s2') subst_acc
                ) subst0 cs1 cs2
    | TConstructor(_, x1, args1), TConstructor(_, x2, args2) when x1 = x2 && List.length args1 = List.length args2 ->
        (* budget ignored, as {!TyUtils.tyF_equiv} does *)
        List.fold_left2 (fun s a1 a2 ->
            let a1' = unify_subst_tyF s a1 in
            let a2' = unify_subst_tyF s a2 in
            unify_compose_subst (unify g a1' a2') s
        ) [] args1 args2
    (* nothing outside a ∀ may be substituted for what it binds, so two schemes
       unify only when they already are the same type, which {!tyF_equiv} decides
       up to renaming *)
    | TForAll _, TForAll _ -> if tyF_equiv g [] t1 t2 then [] else raise (Fail "Cannot unify.")
    | TDeclr x1, TDeclr x2 when x1 = x2 -> []
    | _ -> raise (Fail "Cannot unify.")

(* resolved through Γ first, as {!tyS_equiv} is, so a session named by a
   declaration still unifies with the one it names *)
and unify_tyS g s1 s2 = unify_tyS_rec g [] s1 s2

(* [renv] pairs up the μ binders, as {!TyUtils.tyS_equiv} does, rather than
   renaming one side onto the other -- a rename stops at a binder that would
   capture it, leaving the occurrences below unrenamed and so unmatchable *)
and unify_tyS_rec g renv s1 s2 =
    match resolve_declr g s1, resolve_declr g s2 with
    | STUnit, STUnit -> []
    | STSendF(t1a, t2a), STSendF(t1b, t2b)
    | STRecvF(t1a, t2a), STRecvF(t1b, t2b) ->
        let subst1 = unify g t1a t1b in
        let subst2 = unify_tyS_rec g renv (unify_subst_tyS subst1 t2a) (unify_subst_tyS subst1 t2b) in
        unify_compose_subst subst2 subst1
    | STSendS(t1a, t2a), STSendS(t1b, t2b)
    | STRecvS(t1a, t2a), STRecvS(t1b, t2b) ->
        let subst1 = unify_tyS_rec g renv t1a t1b in
        let subst2 = unify_tyS_rec g renv (unify_subst_tyS subst1 t2a) (unify_subst_tyS subst1 t2b) in
        unify_compose_subst subst2 subst1
    | STExtChoice ls1, STExtChoice ls2
    | STIntChoice ls1, STIntChoice ls2 ->
        if List.map fst ls1 <> List.map fst ls2 then raise (Fail "Cannot unify.")
        else
            List.fold_left2
                (fun subst_acc (_, st1) (_, st2) ->
                    let st1' = unify_subst_tyS subst_acc st1 in
                    let st2' = unify_subst_tyS subst_acc st2 in
                    unify_compose_subst (unify_tyS_rec g renv st1' st2') subst_acc
                ) [] ls1 ls2
    | STRec(_, x1, st1), STRec(_, x2, st2) -> unify_tyS_rec g ((x1, x2)::renv) st1 st2
    | STRecVar x1, STRecVar x2 when paired renv x1 x2 -> []
    | STDeclr x1, STDeclr x2 when x1 = x2 -> [] (* only reached when Γ defines neither *)
    | _ -> raise (Fail "Cannot unify.")
