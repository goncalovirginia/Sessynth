open Language
open Printer
open ChoiceUtils.Let_syntax

module Language = Language

exception Fail of string

(* records *)

type fresh_indices = { id : int; func : int; chan : int; kind : int }

type flags = { 
    xRecLam : id; 
    freshIndices : fresh_indices; 
    currDepth : int; 
    maxDepth : int; 
    printDebug : bool 
}

type bindingsS = (id * tyS) list
type bindingsF = (id * tyF) list
type constructors = bindingsF (* x_c : ∀ᾱ. τ1 -> ... -> τn -> T ᾱ *)

type gamma = { a : bindingsS; s : bindingsS }
    
type psi = { a : bindingsF; s : bindingsF; c : constructors }
    
type delta = { a : bindingsS; s : bindingsS }
    
type contexts = { g : gamma; p : psi; d : delta; }

(* auxiliary functions *)

let initialize_flags maxDepth printDebug =
    { 
        xRecLam = ""; 
        freshIndices = { id = 0; func = 0; chan = 0; kind = 0 }; 
        currDepth = -1; 
        maxDepth = maxDepth; 
        printDebug = printDebug 
    }

let initialize_ctxts =
    let g : gamma = { a = []; s = [] } in
    let p : psi = { a = []; s = []; c = [] } in
    let d : delta = { a = []; s = [] } in
    let ctxts : contexts = { g = g; p = p; d = d } in
    ctxts

let fresh_id f =
    let curr_id = f.freshIndices.id in
    let f' = { f with freshIndices = { f.freshIndices with id = curr_id + 1 } } in
    f', ("_x" ^ string_of_int curr_id)

let fresh_fun f =
    let curr_fun = f.freshIndices.func in
    let f' = { f with freshIndices = { f.freshIndices with func = curr_fun + 1 } } in
    f', ("_f" ^ string_of_int curr_fun)

let fresh_chan f =
    let curr_chan = f.freshIndices.chan in
    let f' = { f with freshIndices = { f.freshIndices with chan = curr_chan + 1 } } in
    f', ("_c" ^ string_of_int curr_chan)

let fresh_kind f =
    let curr_kind = f.freshIndices.kind in
    let f' = { f with freshIndices = { f.freshIndices with kind = curr_kind + 1 } } in
    f', ("_α" ^ string_of_int curr_kind)

let increment_depth f = 
    if f.currDepth < f.maxDepth then Choice.return { f with currDepth = f.currDepth + 1 }
    else Choice.fail 

let debugF f ctxts goal tFocus curr_fun =
    if not f.printDebug then ()
    else let indent = String.make (f.currDepth * 2) ' ' in
        print_endline (indent ^ "currDepth: " ^ string_of_int f.currDepth); 
        match curr_fun with
        | "invertRightF" | "invertLeftF" | "focusDecideF" ->
            print_endline (indent ^ curr_fun ^ ": " ^ tyF_to_string goal);
            print_endline (indent ^ "pa: " ^ psi_to_string ctxts.p.a);
            print_endline (indent ^ "ps: " ^ psi_to_string ctxts.p.s)
        | _ ->
            print_endline (indent ^ curr_fun ^ ": " ^ tyF_to_string goal);
            print_endline (indent ^ "tFocus: " ^ tyF_to_string tFocus)

let debugS f ctxts goal tFocus curr_fun =
    if not f.printDebug then ()
    else let indent = String.make (f.currDepth * 2) ' ' in
        print_endline (indent ^ "currDepth: " ^ string_of_int f.currDepth); 
        match curr_fun with
        | "invertRightS" | "invertLeftS" | "focusDecideS" ->
            print_endline (indent ^ curr_fun ^ ": " ^ tyS_to_string goal);
            print_string (indent ^ "da: " ^ delta_to_string ctxts.d.a);
            print_string (indent ^ "ds: " ^ delta_to_string ctxts.d.s)
        | _ ->
            print_endline (indent ^ curr_fun ^ ": " ^ tyS_to_string goal);
            print_endline (indent ^ "tFocus: " ^ tyS_to_string tFocus)

let is_tyF_left_async t = 
    match t with
    | TConstructor _ -> true
    | _ -> false

let is_tyS_left_async t =
    match t with
    | STSendF _ | STSendS _ | STUnit | STIntChoice _ -> true
    | _ -> false

let rec append_bindings ctxt bindings is_left_async_func =
    let ctxta, ctxts = ctxt in
    match bindings with
    | (x, t)::bindings' ->
        if is_left_async_func t then append_bindings ((x, t)::ctxta, ctxts) bindings' is_left_async_func
        else append_bindings (ctxta, (x, t)::ctxts) bindings' is_left_async_func
    | [] -> ctxt

let append_bindings_gamma ctxts bindings =
    let ga', gs' = append_bindings (ctxts.g.a, ctxts.g.s) bindings is_tyS_left_async in
    { ctxts with g = { a = ga'; s = gs' } }

let append_bindings_psi ctxts bindings = 
    let pa', ps' = append_bindings (ctxts.p.a, ctxts.p.s) bindings is_tyF_left_async in
    { ctxts with p = { a = pa'; s = ps'; c = ctxts.p.c } }

let append_bindings_delta ctxts bindings =
    let da', ds' = append_bindings (ctxts.d.a, ctxts.d.s) bindings is_tyS_left_async in
    { ctxts with d = { a = da'; s = ds' } }

let deltas_are_equal ctxtsl = 
    let hdCtxts = List.hd ctxtsl in
    let are_equal = List.for_all (fun currCtxts -> 
        List.equal (=) hdCtxts.d.a currCtxts.d.a && 
        List.equal (=) hdCtxts.d.s currCtxts.d.s
    ) (List.tl ctxtsl) in
    if are_equal then Some hdCtxts else None

let delta_is_empty ctxts =
    List.is_empty ctxts.d.a && List.is_empty ctxts.d.s

let rec subst e1 x e2 =
    match e1 with 
    | Var y -> if x=y then e2 else e1 
    | App(e, e') -> App (subst e x e2 , subst e' x e2)
    | Lam(y, t, e) -> if x <> y then Lam (y,t,(subst e x e2)) else e1 
    | _ -> raise (Fail("subst pattern matching not defined for " ^ expF_to_string e1 0))

let get_keys kvl =
    List.map (fun (k, v) -> k) kvl

let get_and_remove k kvl =
    let v = List.assoc k kvl in
    let kvl' = List.remove_assoc k kvl in
    kvl', v

let consume_channel ctxts c =
    try let da', s = get_and_remove c ctxts.d.a in
        { ctxts with d = { ctxts.d with a = da'} }, s
    with Not_found ->
        let ds', s = get_and_remove c ctxts.d.s in
        { ctxts with d = { ctxts.d with s = ds'} }, s

let get_first_delta_binding ctxts =
    try List.hd ctxts.d.a
    with Failure _ -> try List.hd ctxts.d.s
    with Failure _ -> raise (Fail("get_first_channel: delta context empty"))

let consume_channels ctxts cl =
    let rec consume_channels' ctxts' cl' sl =
        match cl' with
        | [] -> ctxts', sl
        | c'::cl'' -> 
            let ctxts'', s = consume_channel ctxts' c' in
            consume_channels' ctxts'' cl'' (sl@[s])
    in consume_channels' ctxts cl []

let rec consume_channels_by_tyS ctxts tSl =
    match tSl with
    | [] -> Choice.return (ctxts, [])
    | tS::tSl' ->
        let csl = List.filter (fun (_, s) -> s = tS) ctxts.d.a @
            List.filter (fun (_, s) -> s = tS) ctxts.d.s in
        let* (c, _) = Choice.of_list csl in
        let ctxts', _ = consume_channel ctxts c in
        let* (ctxts'', cl) = consume_channels_by_tyS ctxts' tSl' in
        Choice.return (ctxts'', c::cl)

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
    | TProcess(csl, _) -> List.map (fun (c, s) -> s) csl
    | _ -> raise (Fail("get_TProcess_insl: t not of type TProcess"))

let get_TProcess_outs t =
    match t with
    | TProcess(_, outs) -> outs
    | _ -> raise (Fail("get_TProcess_outs: t not of type TProcess"))

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

let find_binding_for_ty bl bt =
    let (x, _) = List.find (fun (_, t) -> t = bt) bl in x

let find_binding_for_tyF ctxts tF =
    try find_binding_for_ty ctxts.p.a tF
    with Not_found -> find_binding_for_ty ctxts.p.s tF

let rec subst_continuation_exp exp cont_exp =
    match exp with
    | SendF(c, e1, e2) -> SendF(c, e1, subst_continuation_exp e2 cont_exp)
    | RecvF(c, t, e1, e2) -> RecvF(c, t, e1, subst_continuation_exp e2 cont_exp)
    | SendS(c1, c2, e1, e2) -> SendS(c1, c2, e1, subst_continuation_exp e2 cont_exp)
    | RecvS(c, t, e1, e2) -> RecvS(c, t, e1, subst_continuation_exp e2 cont_exp)
    | Wait(c, e) -> Wait(c, subst_continuation_exp e cont_exp)
    | ChoiceSelect(c, l, e) -> ChoiceSelect(c, l, subst_continuation_exp e cont_exp)
    | Spawn(cSpawn, eApp, cl, e) -> Spawn(cSpawn, eApp, cl, subst_continuation_exp e cont_exp )
    | _ -> cont_exp

let get_Spawn_c e =
    match e with
    | Spawn(c, _, _, _) -> c
    | _ -> raise (Fail("get_Spawn_c: e not Spawn expression"))

let filter_duplicates expl =
    let rec filter_duplicates' seen rest =
        match rest with
        | [] -> List.rev seen
        | e::rest' -> 
            if List.exists (fun e' -> e = e') seen then filter_duplicates' seen rest'
            else filter_duplicates' (e :: seen) rest'
    in 
    filter_duplicates' [] expl

let contains_substring s1 s2 =
    let re = Str.regexp_string s2 in
    try ignore (Str.search_forward re s1 0); true
    with Not_found -> false

let filter_ext_choice_solutions_by_substr substr solutions =
  List.filter (fun (_, (_, _, e)) -> contains_substring (expP_to_string e 0) substr) solutions

let rec interactive_ext_choice_filter_solutions solutions =
    print_string "Enter a filter string (or nothing to keep all): ";
    let filter = read_line () in
    if filter = "" then solutions
    else
        let filtered_solutions = filter_ext_choice_solutions_by_substr filter solutions in
        if filtered_solutions = [] then begin
            print_endline "No solutions matched that filter.";
            interactive_ext_choice_filter_solutions solutions 
        end
        else begin
            print_endline ("Filtered down to " ^ string_of_int (List.length filtered_solutions) ^ " solutions:");
            List.iteri (fun i (_, (_, _, e)) -> Printf.printf "%d:\n%s\n" i (expP_to_string e 0)) filtered_solutions;
            interactive_ext_choice_filter_solutions filtered_solutions
        end

(* S[μt.S / t] *)
let rec unfold tUnfold stRec stRecVar =
    match tUnfold with
    | STSendF(t1, t2) ->
        STSendF(t1, unfold t2 stRec stRecVar)
    | STRecvF(t1, t2) ->
        STRecvF(t1, unfold t2 stRec stRecVar)
    | STSendS(t1, t2) ->
        STSendS (unfold t1 stRec stRecVar, unfold t2 stRec stRecVar)
    | STRecvS(t1, t2) ->
        STRecvS (unfold t1 stRec stRecVar, unfold t2 stRec stRecVar)
    | STUnit ->
        STUnit
    | STExtChoice labelsesslist ->
        STExtChoice (List.map (fun (l, t) -> (l, unfold t stRec stRecVar)) labelsesslist)
    | STIntChoice labelsesslist ->
        STIntChoice (List.map (fun (l, t) -> (l, unfold t stRec stRecVar)) labelsesslist)
    | STRec(k, x, t) ->
        if x = stRecVar then tUnfold (* prevents infinite unfolding if for some reason μt.S is used inside the original μt.S *)
        else STRec(k, x, unfold t stRec stRecVar) (* in case another recursive session type μt2.S2 is used inside μt.S *)
    | STRecVar(x) ->
        if x = stRecVar then stRec (* where unfolding happens *)
        else tUnfold (* if it's another recursive name t2 != t, simply return t2 *)
    | _ -> raise (Fail "unfold: unexpected STDeclr while unfolding recursive session-type")

(* polymorphism *)

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
    List.fold_left (fun s (_, t) -> S.union s (ftv_tyF t)) S.empty (ctxts.p.a @ ctxts.p.s)

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

(* ∀ᾱ. F → F[β̄/ᾱ] *)
let instantiate_tyF f t =
    match t with
    | TForAll (xkl, tF) ->
        let () =
            if List.exists (fun (_, k) -> k <> KBase) xkl then
                raise (Fail "instantiate_tyF: only KBase polymorphism is supported")
        in
        let f, subst =
            List.fold_left (fun (f', subst') (a, k) ->
                let f'', k = fresh_kind f' in
                f'', (a, TPolyVar(k))::subst'
            ) (f, []) xkl
        in
        f, instantiate_subst_tyF subst tF
    | _ -> f, t

(* F → ∀ᾱ.F *)
let generalize ctxts t =
    let ftv_t = ftv_tyF t in
    let ftv_env = ftv_env ctxts in
    let vars = S.elements (S.diff ftv_t ftv_env) in
    if vars = [] then t
    else TForAll (List.map (fun a -> (a, KBase)) vars, t)

let rec unify_subst s t =
    match t with
    | TAtomic(TPolyVar a) -> begin 
        match List.assoc_opt a s with
        | Some t' -> t'
        | None -> t
        end
    | TAtomic _ -> t
    | TArrow(t1, t2) -> TArrow (unify_subst s t1, unify_subst s t2)
    | TRefinement(x, a, r) -> TRefinement (x, a, r)
    | TProcess(cs, s') -> TProcess (List.map (fun (c, st) -> (c, unify_subst_tyS s st)) cs, unify_subst_tyS s s')
    | TDeclr(x) -> TDeclr x
    | TForAll(xks, t') ->
        let bound = List.map fst xks in
        let s' = List.filter (fun (a, _) -> not (List.mem a bound)) s in
        TForAll (xks, unify_subst s' t')
    | TConstructor(x, args) -> TConstructor(x, List.map (fun arg -> unify_subst s arg) args)

and unify_subst_tyS s t =
    match t with
    | STSendF(t1, t2) -> STSendF(unify_subst s t1, unify_subst_tyS s t2)
    | STRecvF(t1, t2) -> STRecvF(unify_subst s t1, unify_subst_tyS s t2)
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
    let existing_binding_ids = List.map fst (ctxts.p.a @ ctxts.p.s) in
    let new_bindings = List.filter (fun (a, _) -> not (List.mem a existing_binding_ids)) subst in
    let ctxts' = append_bindings_psi ctxts new_bindings in
    let pa'' = List.map (fun (x, t) -> (x, unify_subst subst t)) ctxts'.p.a in
    let ps'' = List.map (fun (x, t) -> (x, unify_subst subst t)) ctxts'.p.s in
    let pc'' = List.map (fun (x, t) -> (x, unify_subst subst t)) ctxts'.p.c in
    { ctxts with p = { a = pa''; s = ps''; c = pc'' } }

(* apply substitution to entire substitution set *)
let unify_subst_subst s1 s2 =
    List.map (fun (x, t) -> (x, unify_subst s1 t)) s2

(* apply s2, then s1 *)
let unify_compose_subst s1 s2 =
    s1 @ unify_subst_subst s1 s2

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
    | TConstructor(x, args) -> List.exists (fun arg -> occurs a arg) args

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
        if t = TAtomic(TPolyVar(a)) then []
        else if occurs a t then raise (Fail "Occurs check failed.")
        else [(a, t)]
    | TRefinement(_, a1, _), TRefinement(_, a2, _) -> unify (TAtomic a1) (TAtomic a2)
    | TRefinement(_, a, _), t | t, TRefinement(_, a, _) -> unify (TAtomic a) t
    | TArrow(a1, b1), TArrow(a2, b2) ->
        let s1 = unify a1 a2 in
        let s2 = unify (unify_subst s1 b1) (unify_subst s1 b2) in
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
            let a1' = unify_subst s a1 in
            let a2' = unify_subst s a2 in
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

(* ADTs *)

let constructors_of constructors c_T =
    List.filter (fun (c, tF) -> 
        match tF with
        | TForAll(_, tF') -> let _, c_T' = flatten_TArrow tF' in c_T' = c_T
        | _ -> raise (Fail "constructors_of: binding in constructors ctxt not a TForAll.")
    ) constructors

(* inversion and focusing *)

(* flags + gamma-async; gamma-sync; psi-async; psi-async; delta-async; delta-sync |- P :: c : goal *)

let rec invert_right_F f ctxts goal =
    let* f = increment_depth f in
    debugF f ctxts goal goal "invertRightF";
    match goal with
    | TArrow(t1, t2) ->
        let f, x = match t1 with
            | TRefinement(x, _, _) -> f, x
            | _ -> fresh_id f in 
        let ctxts1 = append_bindings_psi ctxts [(x, t1)] in
        begin match get_return_type t2 with
        | TProcess(_, STRec _) when not (List.mem_assoc f.xRecLam ctxts.p.s) ->
            let f, ctxts2 =
            try
                let (xRecFun, _) = List.find (fun (_, t) -> t = goal) ctxts.p.s in
                let f = { f with xRecLam = xRecFun } in
                f, ctxts1
            with Not_found ->
                let f, fresh_f = fresh_fun f in
                let f = { f with xRecLam = fresh_f } in
                let ctxts2 = append_bindings_psi ctxts1 [(f.xRecLam, goal)] in
                f, ctxts2
            in
            let* (f, ctxts', e) = invert_right_F f ctxts2 t2 in
            Choice.return (f, ctxts', LetRec(f.xRecLam, goal, Lam(x, t1, e)))
        | _ -> 
            let* (f, ctxts', e) = invert_right_F f ctxts1 t2 in
            Choice.return (f, ctxts', Lam(x, t1, e))
        end
    | TProcess(incsl, outs) ->
            let f = if f.xRecLam = "" then try { f with xRecLam = find_binding_for_tyF ctxts goal } with Not_found -> f else f in
            let ctxts1 = append_bindings_delta ctxts incsl in
            let f, c = fresh_chan f in
            let* (f, ctxts', e) = invert_right_S f ctxts1 c outs in
            let incsl_consumed = List.for_all (fun (c', _) -> List.assoc_opt c' ctxts'.d.a = None && List.assoc_opt c' ctxts'.d.s = None) incsl in
            let* () = Choice.guard incsl_consumed in
            Choice.return (f, ctxts', Process(c, e, outs, incsl))
    | TDeclr(x) ->
        begin try
            let t = List.assoc x ctxts.p.s in 
            invert_right_F f ctxts t
        with Not_found -> Choice.fail
        end
    | TRefinement(x, t1, t2) ->
        let solution = Cvc5adapter.solve ctxts.p.s goal in
        Choice.return (f, ctxts, solution)
    | TForAll(xkl, t) -> 
        let f, t = instantiate_tyF f goal in
        invert_right_F f ctxts t 
    | _ -> invert_left_F f ctxts goal

and invert_right_S f ctxts c goal = 
    let* f = increment_depth f in
    debugS f ctxts goal goal "invertRightS";
    match goal with 
    | STRecvF(t1, t2) ->
        let f, x1 = fresh_id f in
        let ctxts1 = append_bindings_psi ctxts [(x1, t1)] in
        let* (f, ctxts', e) = invert_right_S f ctxts1 c t2 in
        Choice.return (f, ctxts', RecvF(x1, t1, c, e))
    | STRecvS(t1, t2) -> 
        let f, c1 = fresh_chan f in
        let ctxts1 = append_bindings_delta ctxts [(c1, t1)] in
        let* (f, ctxts', e) = invert_right_S f ctxts1 c t2 in
        let c1_consumed = List.assoc_opt c1 ctxts'.d.a = None && List.assoc_opt c1 ctxts'.d.s = None in
        let* () = Choice.guard c1_consumed in
        Choice.return (f, ctxts', RecvS(c1, t1, c, e))
    | STExtChoice(labelsesslist) ->
        (*let synth_branch = fun (l, s) ->
            let* (f', ctxts', eP) = invertRightS f ctxts c s in
            Choice.return (l, (f', ctxts', eP))
        in
        let* branches = ChoiceUtils.map_list synth_branch labelsesslist in
        let ctxtsl = List.map (fun (_, (_, ctxts', _)) -> ctxts') branches in
        let ctxts' = List.hd ctxtsl in
        let ctxts_equal = List.for_all (fun ctxtsn -> ctxtsn.d = ctxts'.d) (List.tl ctxtsl) in
        let* () = Choice.guard ctxts_equal in
        let labelproclist = List.map (fun (l, (_, _, e)) -> (l, e)) branches in
        Choice.return (f, ctxts', Choice(c, labelproclist))*)
        let branches = synth_interactive_ext_choice f ctxts c goal labelsesslist in
        let labelproclist = List.map (fun (l, (_, _, e)) -> (l, e)) branches in
        let (_, (f', ctxts', _)) = List.hd branches in
        Choice.return (f', ctxts', Choice(c, labelproclist))
    | STRec(k, x, t) ->
        if k <= 0 then try
            let* (f, ctxts, eWander) = wander f ctxts c goal in
            Choice.mplus
                (
                (* wander + fwd *)
                let synthFwdCombination = fun (c', t') -> synth_fwd f ctxts c' c t' in
                let* (f, ctxts', eFwd) = Choice.of_list (List.map synthFwdCombination ctxts.d.s) in
                let eWanderAndFwd = subst_continuation_exp eWander eFwd in
                Choice.return (f, ctxts', eWanderAndFwd)
                )
                (
                (* wander + spawn + fwd *)
                let tRecLam = List.assoc f.xRecLam ctxts.p.s in
                let tProcess = get_return_type tRecLam in
                let* (f, ctxts', eSpawn) = focus_left_TProcess f ctxts c (Some tProcess) in
                let f, ctxts'', eFwd = synth_fwd f ctxts' (get_Spawn_c eSpawn) c t in
                let eSpawnAndFwd = subst_continuation_exp eSpawn eFwd in
                let eWanderAndSpawn = subst_continuation_exp eWander eSpawnAndFwd in
                Choice.return (f, ctxts'', eWanderAndSpawn)
                )
            with Not_found -> Choice.fail
        else 
            let tUnfolded = unfold t (STRec(k-1, x, t)) x in
            invert_right_S f ctxts c tUnfolded
    | STDeclr(x) ->
        begin try
            let t = List.assoc x ctxts.d.s in 
            invert_right_S f ctxts c t
        with Not_found -> Choice.fail
        end
    | _ -> invert_left_S f ctxts c goal

and invert_left_F f ctxts goal =
    let* f = increment_depth f in
    debugF f ctxts goal goal "invertLeftF";
    match ctxts.p.a with
    | (x, t)::pa' ->
        let ctxts = { ctxts with p = { ctxts.p with a = pa' } } in
        begin match t with 
        | TConstructor(x, args) ->
            let synth_constructor_branch (x_c, tF) =
                (* instantiate ∀ᾱ. τ1 -> ... -> τn -> T ᾱ *)
                let f, ty_inst = instantiate_tyF f tF in
                let args, res = flatten_TArrow ty_inst in
                (* unify res with t + subst *)
                let subst = unify res t in
                let args' = List.map (unify_subst subst) args in
                let goal' = unify_subst subst goal in
                let ctxts' = unify_subst_ctxts subst ctxts in
                (* introduce fresh bindings for constructor args *)
                let rec fresh_args f acc_ids acc_tys args =
                    match args with
                    | [] -> (f, List.rev acc_ids, List.rev acc_tys)
                    | a::as' ->
                        let f, xi = fresh_id f in
                        fresh_args f (xi::acc_ids) (a::acc_tys) as'
                in
                let f, x_args, t_args = fresh_args f [] [] args' in
                (* extend psi *)
                let bindings = List.combine x_args t_args in
                let ctxts'' = append_bindings_psi ctxts' bindings in
                (* synthesize branch body *)
                let* (f, ctxts''', e_branch) = focus_right_F f ctxts'' goal' in
                Choice.return (f, ctxts''', x_c, x_args, e_branch)
            in 
            let x_constructors = constructors_of ctxts.p.c goal in
            let* () = Choice.guard (not (List.is_empty x_constructors)) in
            let* branches = ChoiceUtils.map_list synth_constructor_branch x_constructors in
            let ctxtsl = List.map (fun (_, ctxts', _, _, _) -> ctxts') branches in
            let* () = Choice.guard (Option.is_some (deltas_are_equal ctxtsl)) in
            let branches = List.map (fun (_, _, x_c, x_args, e_branch) -> (x_c, x_args, e_branch)) branches in
            Choice.return (f, List.hd ctxtsl, Match(Var(x), branches))
        | _ -> Choice.fail
        end
    | [] -> focus_decide_F f ctxts goal 

and invert_left_S f ctxts c goal =
    let* f = increment_depth f in
    debugS f ctxts goal goal "invertLeftS";
    match ctxts.d.a with
    | (c', t')::da' ->
        let ctxts = { ctxts with d = { ctxts.d with a = da' } } in
        begin match t' with 
        | STSendF(t1, t2) ->
            let f, x = fresh_id f in
            let ctxts1 = append_bindings_psi ctxts [(x, t1)] in
            let ctxts2 = append_bindings_delta ctxts1 [(c', t2)] in
            let* (f, ctxts', e) = invert_left_S f ctxts2 c goal in
            let c'_consumed = List.assoc_opt c' ctxts'.d.a = None && List.assoc_opt c' ctxts'.d.s = None in
            let* () = Choice.guard c'_consumed in
            Choice.return (f, ctxts', RecvF(x, t1, c', e))
        | STSendS(t1, t2) ->
            let f, c1 = fresh_chan f in
            let ctxts1 = append_bindings_delta ctxts [(c1, t1); (c', t2)] in
            let* (f, ctxts', e) = invert_left_S f ctxts1 c goal in
            let channels_consumed = 
                List.assoc_opt c1 ctxts'.d.a = None && List.assoc_opt c1 ctxts'.d.s = None &&
                List.assoc_opt c' ctxts'.d.a = None && List.assoc_opt c' ctxts'.d.s = None
            in
            let* () = Choice.guard channels_consumed in
            Choice.return (f, ctxts', RecvS(c1, t1, c', e))
        | STUnit -> 
            let* (f, ctxts', e) = invert_left_S f ctxts c goal in
            Choice.return (f, ctxts', Wait(c', e))
        | STIntChoice(labelsesslist) ->
            let synth_branch f (l, s) =
                let f, cn = fresh_chan f in
                let ctxtsn = append_bindings_delta ctxts [(cn, s)] in
                let* (f', ctxts', eP) = invert_left_S f ctxtsn c goal in
                let cn_consumed = List.assoc_opt cn ctxts'.d.a = None && List.assoc_opt cn ctxts'.d.s = None in
                let* () = Choice.guard cn_consumed in
                Choice.return (f', (l, (f', ctxts', eP)))
            in
            let* (_, branches) = ChoiceUtils.map_list_state f synth_branch labelsesslist in
            let ctxtsl = List.map (fun (_, (_, ctxts', _)) -> ctxts') branches in
            let* () = Choice.guard (Option.is_some (deltas_are_equal ctxtsl)) in
            let labelproclist = List.map (fun (l, (_, _, e)) -> (l, e)) branches in
            Choice.return (f, List.hd ctxtsl, Choice(c', labelproclist))
        | _ -> Choice.fail
        end
    | [] -> focus_decide_S f ctxts c goal

and focus_decide_F f ctxts goal = 
    debugF f ctxts goal goal "focusDecideF";
    let left_focuses = List.map (fun (xFoc, tFoc) -> focus_left_F f ctxts xFoc tFoc goal) ctxts.p.s in
    let right_focus = focus_right_F f ctxts goal in
    ChoiceUtils.mplus_list (left_focuses @ [right_focus])

and focus_decide_S f ctxts c goal = 
    debugS f ctxts goal goal "focusDecideS";
    let left_focuses = List.map (fun (xFoc, tFoc) -> focus_left_S f ctxts xFoc tFoc c goal) ctxts.d.s in
    let right_focus = focus_right_S f ctxts c goal in
    ChoiceUtils.mplus_list (left_focuses @ [right_focus])

and focus_right_F f ctxts goal =
    let* f = increment_depth f in
    debugF f ctxts goal goal "focusRightF";
    match goal with
    | TAtomic(TInt) -> Choice.return (f, ctxts, Int(1))
    | TAtomic(TBool) -> Choice.return (f, ctxts, Bool(true))
    | TAtomic(TPolyVar(_)) ->
        let ground_atomic_types = [TInt; TBool] in
        let map_unify = fun tA ->
            try let candidate = TAtomic(tA) in
                let subst = unify goal candidate in
                let ctxts' = append_bindings_psi ctxts subst in
                let goal' = unify_subst subst goal in
                focus_right_F f ctxts' goal'
            with Fail _ -> Choice.fail
        in
        ChoiceUtils.map_mplus_list map_unify ground_atomic_types
    | TConstructor(x, args) ->
        let synth_constructor_select (x_c, tF) = 
            (* instantiate ∀ᾱ. τ1 -> ... -> τn -> T ᾱ *)
            let f, t_inst = instantiate_tyF f tF in
            let args, res = flatten_TArrow t_inst in
            (* unify res with goal + subst *)
            let subst = unify res goal in
            let args' = List.map (unify_subst subst) args in
            let ctxts' = unify_subst_ctxts subst ctxts in
            (* synthesize constructor arguments *)
            let rec synth_args f ctxts acc args =
                match args with
                | [] -> Choice.return (f, ctxts, List.rev acc)
                | a::args' ->
                    let* (f, ctxts, e) = focus_right_F f ctxts a in
                    synth_args f ctxts (e::acc) args'
            in
            let* (f, ctxts, e_args) = synth_args f ctxts' [] args' in
            Choice.return (f, ctxts, Constructor(x_c, e_args))
        in
        let x_constructors = constructors_of ctxts.p.c goal in
        ChoiceUtils.map_mplus_list synth_constructor_select x_constructors
    | _ -> invert_right_F f ctxts goal

and focus_right_S f ctxts c goal =
    let* f = increment_depth f in
    debugS f ctxts goal goal "focusRightS";
    match goal with 
    | STSendF(t1, t2) ->
        let* (f, ctxts', e1) = focus_decide_F f ctxts t1 in
        let* (f, ctxts'', e2) = focus_right_S f ctxts' c t2 in
        Choice.return (f, ctxts'', SendF(c, e1 , e2))
    | STSendS(t1, t2) -> 
        let f, c' = fresh_chan f in
        let* (f, ctxts', e1) = focus_decide_S f ctxts c' t1 in
        let* (f, ctxts'', e2) = focus_right_S f ctxts' c t2 in
        Choice.return (f, ctxts'', SendS(c, c', e1 , e2))
    | STUnit ->
        let* () = Choice.guard (delta_is_empty ctxts) in
        Choice.return (f, ctxts, Close(c))
    | STIntChoice(labelsesslist) -> 
        let synth_choice_select (l, s) =
            let* (f', ctxts', e1) = focus_right_S f ctxts c s in
            Choice.return (f', ctxts', ChoiceSelect(c, l, e1))
        in
        ChoiceUtils.map_mplus_list synth_choice_select labelsesslist
    | _ -> invert_right_S f ctxts c goal (* goal is not right sync, therefore switch back to inversion phase *)

and focus_left_F f ctxts xFocus tFocus goal =
    let* f = increment_depth f in
    debugF f ctxts goal tFocus "focusLeftF";
    match tFocus with
    | TArrow(t1, t2) ->
        let f, c' = fresh_chan f in
        let* (f, ctxts', e2) = focus_left_F f ctxts c' t2 goal in
        let* (f, ctxts'', e1) = invert_right_F f ctxts t1 in
        Choice.return (f, ctxts'', subst e2 c' (App(Var(xFocus), e1)))
    | TProcess _ | TAtomic _ -> 
        if tFocus = goal then Choice.return (f, ctxts, Var(xFocus))
        else Choice.fail
    | TRefinement(x, tA, tR) ->
        if TAtomic(tA) = goal || (is_TRefinement goal && Cvc5adapter.sat ctxts.p.s tFocus goal)
            then Choice.return (f, ctxts, Var(xFocus))
        else Choice.fail
    | TDeclr x ->
        if tFocus = goal || List.assoc_opt x ctxts.p.s = Some goal then Choice.return (f, ctxts, Var(xFocus))
        else Choice.fail
    | TForAll(xkl, t) -> begin
        try let f, t_inst = instantiate_tyF f tFocus in
            let subst = unify t_inst goal in
            let t_inst' = unify_subst subst t_inst in
            let goal' = unify_subst subst goal in
            let ctxts' = unify_subst_ctxts subst ctxts in
            focus_left_F f ctxts' xFocus t_inst' goal'
        with Fail _ -> Choice.fail end
    | _ -> invert_left_F f ctxts goal

and focus_left_S f ctxts cFocus tFocus c goal =
    let* f = increment_depth f in
    debugS f ctxts goal tFocus "focusLeftS";
    match tFocus with
    | STRecvF(t1, t2) ->
        let ctxts0, _ = consume_channel ctxts cFocus in
        let ctxts1 = append_bindings_delta ctxts0 [(cFocus, t2)] in
        let* (f, ctxts', e1) = invert_right_F f ctxts1 t1 in
        let* (f, ctxts'', e2) = focus_left_S f ctxts' cFocus t2 c goal in
        Choice.return (f, ctxts'', SendF(cFocus, e1, e2))
    | STRecvS(t1, t2) -> 
        let f, c' = fresh_chan f in
        let ctxts0, _ = consume_channel ctxts cFocus in
        let ctxts1 = append_bindings_delta ctxts0 [(cFocus, t2)] in
        let* (f, ctxts', e1) = invert_right_S f ctxts1 c' t1 in
        let* (f, ctxts'', e2) = focus_left_S f ctxts' cFocus t2 c goal in
        Choice.return (f, ctxts'', SendS(cFocus, c', e1, e2))
    | STExtChoice(labelsesslist) -> 
        let synth_choice_select (l, s) =
            let ctxts0, _ = consume_channel ctxts cFocus in
            let ctxts1 = append_bindings_delta ctxts0 [(cFocus, s)] in
            let* (f', ctxts', e1) = focus_left_S f ctxts1 cFocus s c goal in
            Choice.return (f', ctxts', ChoiceSelect(cFocus, l, e1))
        in
        ChoiceUtils.map_mplus_list synth_choice_select labelsesslist
    | STRec(k, x, t) ->
        if k <= 0 then
            Choice.return (f, ctxts, Close(""))
        else
            let tUnfolded = unfold t (STRec(k-1, x, t)) x in
            let ctxts0, _ = consume_channel ctxts cFocus in
            let ctxts1 = append_bindings_delta ctxts0 [(cFocus, tUnfolded)] in
            focus_left_S f ctxts1 cFocus tUnfolded c goal
    | _ -> invert_left_S f ctxts c goal

(* wandering *)

and wander f ctxts c goal =
    debugS f ctxts goal goal "wander";
    let skipChoice = Choice.return (f, ctxts, Close("")) in
    let spawnChoice = wander_spawn f ctxts c goal in
    let unfoldChoice = wander_unfold f ctxts c goal in
    ChoiceUtils.mplus_list [skipChoice; spawnChoice; unfoldChoice]

and wander_spawn f ctxts c goal =
    focus_left_TProcess f ctxts c None

and wander_unfold f ctxts c goal =
    let recsessl = List.filter (fun (_, t) -> is_STRec t) ctxts.d.s in
    let synth_unfolds (c', t') = focus_left_S f ctxts c' t' c goal in
    ChoiceUtils.map_mplus_list synth_unfolds recsessl

(* reusable synthesis functions *)

(** 
synthesizes possible spawn expressions which output a desired session-type, containing a placeholder continuation expression
@param goal_tProcess_filter: option possibly containing a TProcess(insl, outs) whose outs serves as a filter for valid spawnable processes with equivalent outs session-types, which will be provided by the spawned channel. If goal_tProcess_filter is None, then any TProcess(_, _) is spawnable
*)
and focus_left_TProcess f ctxts c goal_tProcess_filter =
    let* f = increment_depth f in
    let f, cSpawn = fresh_chan f in
    let spawnable_proc_list = 
        List.filter(fun (_, t) -> 
            let tReturn = get_return_type t in is_TProcess tReturn && (
            (Option.is_none goal_tProcess_filter) ||
            (Option.is_some goal_tProcess_filter && get_TProcess_outs tReturn = get_TProcess_outs (Option.get goal_tProcess_filter)))
        ) ctxts.p.s
    in 
    let* (x, t) = Choice.of_list spawnable_proc_list in
    let tProcess = get_return_type t in
    let insl = get_TProcess_insl tProcess in
    let outs = get_TProcess_outs tProcess in
    let* (f, ctxts', eApp) = focus_left_F f ctxts x t tProcess in
    let* (ctxts'', incl) = consume_channels_by_tyS ctxts' insl in
    let ctxts''' = append_bindings_delta ctxts'' [(cSpawn, outs)] in
    Choice.return (f, ctxts''', Spawn(cSpawn, eApp, incl, Close("")))

and synth_fwd f ctxts cToFwd c t =
    let ctxts', _ = consume_channel ctxts cToFwd in
    f, ctxts', Fwd(cToFwd, c, t)

and synth_interactive_ext_choice f ctxts c goal labelsesslist =
    let synth_branch = fun (l, s) ->
        let* (f', ctxts', eP) = invert_right_S f ctxts c s in
        Choice.return (l, (f', ctxts', eP))
    in
    (* loop over each (label, session) branch in order *)
    let rec iter_labels prev_ctxts_opt picked_solutions labelsesslist =
        match labelsesslist with
        | [] -> List.rev picked_solutions (* all branches handled *)
        | (label, s)::rest ->
            (* synthesize all expressions for the current label *)
            let solutions = synth_branch (label, s) |> Choice.run_all
            in
            (* if we already picked a previous branch, enforce context equality *)
            let compatible_solutions = 
                match prev_ctxts_opt with
                | None -> solutions
                | Some resulting_ctxts ->
                    List.filter (fun (_, (_, ctxts', _)) -> Option.is_some (deltas_are_equal [resulting_ctxts; ctxts'])) solutions
            in
            if List.is_empty compatible_solutions then raise (Fail ("No compatible solutions for label: " ^ label))
            else
                (* print solutions for this branch *)
                print_endline ("\nLabel: " ^ label ^ "\n");
                List.iteri (fun i (_, (_, _, e)) -> Printf.printf "%d:\n%s\n" i (expP_to_string e 0)) compatible_solutions;
                (* filter solutions *)
                let filtered_solutions = interactive_ext_choice_filter_solutions compatible_solutions in
                (* let user pick a solution *)
                let rec pick () =
                    print_string "Select solution: ";
                    match read_int_opt () with
                    | Some i when i >= 0 && i < List.length filtered_solutions -> List.nth filtered_solutions i
                    | _ -> print_endline "Invalid choice.\n"; pick ()
                in
                let (l, (f', ctxts', e)) = pick () in
                (* continue to next branch with the current context as reference *)
                iter_labels (Some ctxts') ((l, (f', ctxts', e))::picked_solutions) rest
        in
        iter_labels None [] labelsesslist

(* entry point *)

let synth n_sol p d goal = 
    let printDebug =
        match Sys.getenv_opt "SESSYNTH_DEBUG" with
        | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
        | _ -> false
    in
    let f, ctxts = initialize_flags 100 printDebug, initialize_ctxts in
    let ctxts = append_bindings_psi ctxts p in
    let ctxts = append_bindings_delta ctxts d in
    let solutions_choice =
        let* (f', ctxts', expF') = invert_right_F f ctxts goal in
        let* () = Choice.guard (delta_is_empty ctxts') in
        Choice.return (f', ctxts', expF')
    in
    let solutions = Choice.run_n n_sol solutions_choice |> List.rev in
    if List.is_empty solutions then raise (Fail "No valid expression for the provided type") else
    let expl = List.map (fun (_, _, e) -> e) solutions in
    let i = ref 0 in
    print_newline ();
    List.iter (fun e -> print_endline (string_of_int !i ^ ":"); print_endline (expF_to_string e 0); incr i) expl;
    if List.length expl = 1 then List.hd expl
    else let rec choose_exp() =
            print_string "Select solution: "; 
            let chosen_exp = read_int() in
            if chosen_exp < 0 || chosen_exp >= List.length expl then choose_exp()
            else List.nth expl chosen_exp
        in choose_exp()

(* Examples and stuff *)

(*
and spawn_rec_process_and_fwd g p d x t c =
    let goal = unfold_TArrow t in
    let g', p', da', ds', spawnedP = focusLeftF g p x t d c goal in
    match goal with
    | TProcess(insl, outs) ->
        let cNew = fresh_channel() in
        let dRemainder, dConsumed = consume_channels_with_sessions d insl [] in
        let inChannels = get_keys dConsumed in
        let dFwd = append_bindings_delta dRemainder [(cNew, outs)] in
        let dFwd', eFwd = fwd_channel dFwd cNew c in
        assert (List.is_empty (fst dFwd') && List.is_empty (snd dFwd'));
        Spawn(cNew, spawnedP, inChannels, eFwd)
    | _ -> raise (Fail "spawn_process: t not of type TProcess")
*)

(*
nats : int -> {rec t . int * t}
let nats = fun x ->
  c <- { 
    // nats: int -> {rec t. int * t} , x:int |- c: rec t. int * t
    // nats: int -> {rec t. int * t} , x:int |- c: int * (rec t. int * t)
    send c 0 ;  
    d <- spawn (nats 0); // nats: int -> {rec t. int * t} , x:int ; empty |- c: t // c: rec t. int * t
	fwd d c // nats: int -> {rec t. int * t} , x:int ; d:rec t.int*t |- c: t // c:rec t . int*t
	}
*)

(*
TRec of id * tyS (* mu t . T *)
TVar of id   (* t *)

!S.T
?S.T

SendInts = mu t . !int.t  ~ !int.(mu t.!int.t)

sendIntsRec : () -> { mu t . !int.t }
let rec sendIntsRec = fun () -> c <- {
  //c:!int. t
  send c 0 ;
  //c: mu t . !int.t
  d <- spawn SendIntsRec () ;
  //d: mu t . !int.t |- c: mu t . !int.t
  fwd d c
}

let rec sendIntsRecBad = fun () -> c  <- {
  d <- spawn sendIntsRecBad () ;
  fwd d c
 }

mu t . &{ l1 => !int.t ; l2 => !string.t ; done => 1 }

let rec foo = fun () -> c <- {
 case c of
  l1 => send c 0 ; d <- spawn foo() ; fwd d c
  l2 => send c "xpto" ; d <- spawn foo() ; fwd d c 
  done => close c
}
*)