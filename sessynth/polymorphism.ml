(* Parametric polymorphism: free type variables, instantiation, generalization
   and first-order unification.

   WORK IN PROGRESS. Only KBase (base-kind) polymorphism is supported;
   instantiate_tyF rejects anything else. See the known gaps around left focus
   on a polymorphic function type. *)

open Language
open TyUtils
open Contexts
open Flags

module S = Set.Make(String)

(* free type variables *)

let ftv_tyA = function
    | TInt | TBool -> S.empty
    | TPolyVar a -> S.singleton a

let rec ftv_tyR = function
    | RTInt _ | RTBool _ | RTVar _ -> S.empty
    | RTUOp (_, r) -> ftv_tyR r
    | RTBOp (_, r1, r2) -> S.union (ftv_tyR r1) (ftv_tyR r2)

let rec ftv_tyF = function
    | TAtomic a -> ftv_tyA a
    | TRefinement(_, a, r) -> S.union (ftv_tyA a) (ftv_tyR r)
    | TArrow(t1, t2) -> S.union (ftv_tyF t1) (ftv_tyF t2)
    | TProcess(_, _) | TDeclr _ -> S.empty
    | TForAll(xkl, t) ->
        let vars_set = List.fold_left (fun s (x, _) -> S.add x s) S.empty xkl in
        S.diff (ftv_tyF t) vars_set
    | TConstructor(_, args) ->
        List.fold_left (fun s t -> S.union s (ftv_tyF t)) S.empty args

let ftv_env ctxts =
    List.fold_left (fun s (_, t) -> S.union s (ftv_tyF t)) S.empty ctxts.p

(* instantiation *)

let instantiate_subst_tyA subst t =
    match t with
    | TInt -> TInt
    | TBool -> TBool
    | TPolyVar a -> try List.assoc a subst with Not_found -> TPolyVar a

let rec instantiate_subst_tyF subst t =
    match t with
    | TAtomic a -> TAtomic (instantiate_subst_tyA subst a)
    | TRefinement (x, a, r) -> TRefinement (x, instantiate_subst_tyA subst a, r)
    | TArrow (t1, t2) -> TArrow (instantiate_subst_tyF subst t1, instantiate_subst_tyF subst t2)
    | TProcess (incsl, outs) -> TProcess (List.map (fun (c, s) -> (c, instantiate_subst_tyS subst s)) incsl, instantiate_subst_tyS subst outs)
    | TDeclr x -> TDeclr x
    | TForAll (xkl, tF) ->
        (* avoid capture: ignore substitution for re-bound vars *)
        let subst' = List.filter (fun (a, _) -> not (List.exists (fun (v, _) -> v = a) xkl)) subst in
        TForAll (xkl, instantiate_subst_tyF subst' tF)
    | TConstructor(x, args) ->
        TConstructor(x, List.map (fun arg -> instantiate_subst_tyF subst arg) args)

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

(* ∀ᾱ. F → F[β̄/ᾱ] *)
let instantiate_tyF f t =
    match t with
    | TForAll (xkl, tF) ->
        let () =
            if List.exists (fun (_, k) -> k <> KBase) xkl then
                raise (Fail "instantiate_tyF: only KBase polymorphism is supported")
        in
        let f, subst =
            List.fold_left (fun (f', subst') (a, _) ->
                let f'', k = fresh_kind f' in
                f'', (a, TPolyVar(k))::subst'
            ) (f, []) xkl
        in
        f, instantiate_subst_tyF subst tF
    | _ -> f, t

(* F → ∀ᾱ.F *)
let generalize ctxts t =
    let ftv_t = ftv_tyF t in
    let ftv_env = ftv_env ctxts in
    let vars = S.elements (S.diff ftv_t ftv_env) in
    if vars = [] then t
    else TForAll (List.map (fun a -> (a, KBase)) vars, t)

(* applying a unifying substitution *)

let unify_subst_tyA s a =
    match a with
    | TInt | TBool -> a
    | TPolyVar v ->
        match List.assoc_opt v s with
        | Some (TAtomic a') -> a'
        | Some (TRefinement(_, a', _)) -> a'
        | Some _ -> raise (Fail "unify_subst: KBase polyvar substituted with non-base type")
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
    | TForAll(xks, t') ->
        let bound = List.map fst xks in
        let s' = List.filter (fun (a, _) -> not (List.mem a bound)) s in
        TForAll (xks, unify_subst_tyF s' t')
    | TConstructor(x, args) -> TConstructor(x, List.map (fun arg -> unify_subst_tyF s arg) args)

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
    | TRefinement(_, _, _) -> false
    | TProcess(cs, s') ->
        List.exists (fun (_, st) -> occurs_tyS a st) cs || occurs_tyS a s'
    | TDeclr _ -> false
    | TForAll(xks, t') ->
        if List.mem a (List.map fst xks) then false else occurs a t'
    | TConstructor(_, args) -> List.exists (fun arg -> occurs a arg) args

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
let rec unify t1 t2 =
    match t1, t2 with
    | TAtomic(TInt), TAtomic(TInt) -> []
    | TAtomic(TBool), TAtomic(TBool) -> []
    | TAtomic(TPolyVar(a)), t | t, TAtomic(TPolyVar(a)) ->
        let t' =
            match t with
            | TRefinement(_, a2, _) -> TAtomic a2
            | _ -> t
        in
        begin
        match t' with
        | TAtomic(TInt) | TAtomic(TBool) | TAtomic(TPolyVar _) ->
            if t' = TAtomic(TPolyVar(a)) then []
            else if occurs a t' then raise (Fail "Occurs check failed.")
            else [(a, t')]
        | _ -> raise (Fail "Cannot unify: KBase polyvar with non-base type")
        end
    | TRefinement(_, a1, _), TRefinement(_, a2, _) -> unify (TAtomic a1) (TAtomic a2)
    | TRefinement(_, a, _), t | t, TRefinement(_, a, _) -> unify (TAtomic a) t
    | TArrow(a1, b1), TArrow(a2, b2) ->
        let s1 = unify a1 a2 in
        let s2 = unify (unify_subst_tyF s1 b1) (unify_subst_tyF s1 b2) in
        unify_compose_subst s2 s1
    | TProcess(cs1, outs1), TProcess(cs2, outs2) ->
        if List.length cs1 <> List.length cs2 then raise (Fail "Cannot unify.")
        else
            let subst0 = unify_tyS outs1 outs2 in
            List.fold_left2
                (fun subst_acc (_, s1) (_, s2) ->
                    let s1' = unify_subst_tyS subst_acc s1 in
                    let s2' = unify_subst_tyS subst_acc s2 in
                    unify_compose_subst (unify_tyS s1' s2') subst_acc
                ) subst0 cs1 cs2
    | TConstructor(x1, args1), TConstructor(x2, args2) when x1 = x2 && List.length args1 = List.length args2 ->
        List.fold_left2 (fun s a1 a2 ->
            let a1' = unify_subst_tyF s a1 in
            let a2' = unify_subst_tyF s a2 in
            unify_compose_subst (unify a1' a2') s
        ) [] args1 args2
    | _ -> raise (Fail "Cannot unify.")

and unify_tyS s1 s2 =
    match s1, s2 with
    | STUnit, STUnit -> []
    | STSendF(t1a, t2a), STSendF(t1b, t2b)
    | STRecvF(t1a, t2a), STRecvF(t1b, t2b) ->
        let subst1 = unify t1a t1b in
        let subst2 = unify_tyS (unify_subst_tyS subst1 t2a) (unify_subst_tyS subst1 t2b) in
        unify_compose_subst subst2 subst1
    | STSendS(t1a, t2a), STSendS(t1b, t2b)
    | STRecvS(t1a, t2a), STRecvS(t1b, t2b) ->
        let subst1 = unify_tyS t1a t1b in
        let subst2 = unify_tyS (unify_subst_tyS subst1 t2a) (unify_subst_tyS subst1 t2b) in
        unify_compose_subst subst2 subst1
    | STExtChoice ls1, STExtChoice ls2
    | STIntChoice ls1, STIntChoice ls2 ->
        if List.map fst ls1 <> List.map fst ls2 then raise (Fail "Cannot unify.")
        else
            List.fold_left2
                (fun subst_acc (_, st1) (_, st2) ->
                    let st1' = unify_subst_tyS subst_acc st1 in
                    let st2' = unify_subst_tyS subst_acc st2 in
                    unify_compose_subst (unify_tyS st1' st2') subst_acc
                ) [] ls1 ls2
    | STRec(_, x1, st1), STRec(_, x2, st2) ->
        let st2' = if x1 = x2 then st2 else rename_recvar_in_tyS x2 x1 st2 in
        unify_tyS st1 st2'
    | STRecVar x1, STRecVar x2 when x1 = x2 -> []
    | STDeclr x1, STDeclr x2 when x1 = x2 -> []
    | _ -> raise (Fail "Cannot unify.")
