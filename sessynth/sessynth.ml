open Language
open Printer
open TyUtils
open Contexts
open Polymorphism
open Adts
open Flags
open ChoiceUtils.Let_syntax

module Language = Language

let resolve_declr = TyUtils.resolve_declr
let tyF_to_string = Printer.tyF_to_string
let expF_to_string = Printer.expF_to_string
let set_max_depth = Flags.set_max_depth

(* re-exported so that Sessynth.Fail keeps naming the same exception *)
exception Fail = TyUtils.Fail

(* debug tracing *)

let debugF f ctxts goal tFocus curr_fun =
    if not !print_debug then ()
    else let indent = String.make (f.depth * 2) ' ' in
        print_endline (indent ^ "depth: " ^ string_of_int f.depth); 
        match curr_fun with
        | "invertRightF" | "invertLeftF" | "focusDecideF" ->
            print_endline (indent ^ curr_fun ^ ": " ^ tyF_to_string goal);
            print_endline (indent ^ "p: " ^ psi_to_string ctxts.p)
        | _ ->
            print_endline (indent ^ curr_fun ^ ": " ^ tyF_to_string goal);
            print_endline (indent ^ "tFocus: " ^ tyF_to_string tFocus)

let debugS f ctxts goal tFocus curr_fun =
    if not !print_debug then ()
    else let indent = String.make (f.depth * 2) ' ' in
        print_endline (indent ^ "depth: " ^ string_of_int f.depth); 
        match curr_fun with
        | "invertRightS" | "invertLeftS" | "focusDecideS" ->
            print_endline (indent ^ curr_fun ^ ": " ^ tyS_to_string goal);
            print_string (indent ^ "d: " ^ delta_to_string ctxts.d)
        | _ ->
            print_endline (indent ^ curr_fun ^ ": " ^ tyS_to_string goal);
            print_endline (indent ^ "tFocus: " ^ tyS_to_string tFocus)

let get_keys kvl =
    List.map (fun (k, _) -> k) kvl

(** Substitutes every [Hole] in [exp] with [cont_exp]. *)
let rec subst_continuation_exp exp cont_exp =
    match exp with
    | Hole _ -> cont_exp
    | SendF(c, eF, eP) -> SendF(c, eF, subst_continuation_exp eP cont_exp)
    | RecvF(x, t, c, eP) -> RecvF(x, t, c, subst_continuation_exp eP cont_exp)
    | SendS(c1, c2, eP1, eP2) -> SendS(c1, c2, eP1, subst_continuation_exp eP2 cont_exp)
    | RecvS(c1, t, c2, eP) -> RecvS(c1, t, c2, subst_continuation_exp eP cont_exp)
    | Wait(c, eP) -> Wait(c, subst_continuation_exp eP cont_exp)
    | ChoiceSelect(c, l, eP) -> ChoiceSelect(c, l, subst_continuation_exp eP cont_exp)
    | Spawn(cSpawn, eApp, cl, eP) -> Spawn(cSpawn, eApp, cl, subst_continuation_exp eP cont_exp)
    | Choice(c, labelproclist) ->
        Choice(c, List.map (fun (l, eP) -> (l, subst_continuation_exp eP cont_exp)) labelproclist)
    | Close _ | Fwd _ -> exp

(* a solution still containing a placeholder is incomplete, not a valid program *)
let rec has_hole exp =
    match exp with
    | Hole _ -> true
    | Close _ | Fwd _ -> false
    | SendF(_, _, eP) | RecvF(_, _, _, eP) | RecvS(_, _, _, eP)
    | Wait(_, eP) | ChoiceSelect(_, _, eP) -> has_hole eP
    | SendS(_, _, eP1, eP2) -> has_hole eP1 || has_hole eP2
    | Spawn(_, eApp, _, eP) -> has_hole_expF eApp || has_hole eP
    | Choice(_, labelproclist) -> List.exists (fun (_, eP) -> has_hole eP) labelproclist

and has_hole_expF exp =
    match exp with
    | Int _ | Bool _ | Var _ -> false
    | UOp(_, e) | Lam(_, _, e) | LetRec(_, _, e) -> has_hole_expF e
    | BOp(_, e1, e2) | Let(_, e1, e2) | App(e1, e2) -> has_hole_expF e1 || has_hole_expF e2
    | Ite(e1, e2, e3) -> has_hole_expF e1 || has_hole_expF e2 || has_hole_expF e3
    | Process(_, eP, _, _) -> has_hole eP
    | Constructor(_, el) -> List.exists has_hole_expF el
    | Match(e, branches) -> has_hole_expF e || List.exists (fun (_, _, e') -> has_hole_expF e') branches

let get_Spawn_c e =
    match e with
    | Spawn(c, _, _, _) -> Some c
    | _ -> None

(** Whether every recursive occurrence in [eP] sits behind a communication on [c]
    (the channel offered). An unguarded one spawns copies of itself forever without
    the conversation advancing. A recursive occurrence is a spawn headed by any of
    [selves], the names the process can recur through; [""] names nothing. *)
let degenerate_recursion_is_guarded selves c eP =
    let is_self eApp =
        match get_app_head_id eApp with
        | Some x -> x <> "" && List.mem x selves
        | None -> false
    in
    let rec guarded eP =
        match eP with
        (* progress on the offered channel: whatever follows is guarded *)
        | SendF(c', _, _) | RecvF(_, _, c', _) | RecvS(_, _, c', _)
        | Wait(c', _) | ChoiceSelect(c', _, _) when c' = c -> true
        | SendS(c', _, _, _) when c' = c -> true
        | Choice(c', branches) when c' = c -> List.for_all (fun (_, e) -> guarded e) branches
        | Close _ | Fwd _ | Hole _ -> true
        (* the recursive occurrence itself, reached before any such progress *)
        | Spawn(_, eApp, _, _) when is_self eApp -> false
        (* anything else acts on some other channel and carries on *)
        | SendF(_, _, eP') | RecvF(_, _, _, eP') | RecvS(_, _, _, eP')
        | Wait(_, eP') | ChoiceSelect(_, _, eP') | Spawn(_, _, _, eP') -> guarded eP'
        | SendS(_, _, eP1, eP2) -> guarded eP1 && guarded eP2
        | Choice(_, branches) -> List.for_all (fun (_, e) -> guarded e) branches
    in
    guarded eP

let contains_substring s1 s2 =
    let re = Str.regexp_string s2 in
    try ignore (Str.search_forward re s1 0); true
    with Not_found -> false

let print_solutions expl =
    List.iteri (fun i e -> print_endline (string_of_int i ^ ":"); print_endline (expF_to_string e 0)) expl

(** Returns [None] at end of input, so a prompt can fall back on a default instead of
   raising End_of_file out of the middle of synthesis *)
let read_line_opt () = try Some (read_line ()) with End_of_file -> None

(** Narrows a solution list by substring, repeatedly, until the user is happy with what is left. *)
let rec interactive_filter_solutions expl =
    print_string "Enter a filter string (or nothing to keep all): ";
    match read_line_opt () with
    | None | Some "" -> expl
    | Some filter ->
        let filtered = List.filter (fun e -> contains_substring (expF_to_string e 0) filter) expl in
        if List.is_empty filtered then begin
            print_endline "No solutions matched that filter.";
            interactive_filter_solutions expl
        end
        else begin
            print_endline ("Filtered down to " ^ string_of_int (List.length filtered) ^ " solutions:");
            print_solutions filtered;
            interactive_filter_solutions filtered
        end

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
        let ctxts1 = append_bindings_psi (with_self ctxts goal) [(x, t1)] in
        begin match get_return_type t2 with
        | TProcess(_, outs) when is_STRec (resolve_declr ctxts.g outs) && not (List.mem_assoc ctxts.xRecLam ctxts.p) ->
            let f, ctxts2 =
            match find_binding_for_tyF ctxts goal with
            | Some xRecFun ->
                f, { ctxts1 with xRecLam = xRecFun }
            | None ->
                let f, fresh_f = fresh_fun f in
                let ctxts2 = append_bindings_psi ctxts1 [(fresh_f, goal)] in
                f, { ctxts2 with xRecLam = fresh_f }
            in
            let* (f, ctxts', e) = invert_right_F f ctxts2 t2 in
            Choice.return (f, restore_scope ctxts ctxts', LetRec(ctxts2.xRecLam, goal, Lam(x, t1, e)))
        | _ ->
            let* (f, ctxts', e) = invert_right_F f ctxts1 t2 in
            Choice.return (f, restore_scope ctxts ctxts', Lam(x, t1, e))
        end
    | TProcess(incsl, outs) ->
            let ctxts =
                if ctxts.xRecLam <> "" then ctxts
                else match find_binding_for_tyF ctxts goal with
                    | Some xRecFun -> { ctxts with xRecLam = xRecFun }
                    | None -> ctxts
            in
            let* () = Choice.guard (find_first_duplicate_name incsl = None) in
            (* Δ is swapped out rather than extended since a process type is a
               functional value that is spawned later against the channels its own
               declaration names, so the caller's channels are not the body's to consume. *)
            let ctxts1 = { (with_self ctxts goal) with d = incsl } in
            let f, c = fresh_chan f in
            let* (f, ctxts', e) = invert_right_S f ctxts1 c outs in
            let* () = Choice.guard (delta_is_empty ctxts') in
            let* () = Choice.guard (degenerate_recursion_is_guarded [ctxts.xRecLam; ctxts1.xSelf] c e) in
            Choice.return (f, ctxts, Process(c, e, outs, incsl))
    | TDeclr(x) ->
        let* t = ChoiceUtils.of_option (List.assoc_opt x ctxts.p) in
        invert_right_F f ctxts t
    | TRefinement _ ->
        (* an infeasible or unparseable SyGuS goal fails this branch rather than aborting the whole search *)
        let solution = 
            try Some (Cvc5adapter.solve !print_debug ctxts.p goal)
            with Cvc5adapter.CVC5Infeasible _ | Cvc5adapter.CVC5ParseError _ | Cvc5adapter.CVC5Error _ -> None
        in
        let* solution = ChoiceUtils.of_option solution in
        Choice.return (f, ctxts, solution)
    | TForAll _ ->
        (* offering a scheme, so its variables stand for types whoever calls has
           already chosen: the body has to serve all of them at once, which it can
           only do by passing a value of that type through. Instantiating here
           instead would let the body pick, answering ∀a. a -> a with _x0 -> 1. *)
        let f, t' = skolemize_tyF f goal in
        invert_right_F f ctxts t'
    | TAtomic _ | TConstructor _ -> invert_left_F f ctxts goal

and invert_right_S f ctxts c goal = 
    let* f = increment_depth f in
    debugS f ctxts goal goal "invertRightS";
    match goal with 
    | STRecvF(t1, t2) ->
        let f, x1 = fresh_id f in
        let ctxts1 = append_bindings_psi ctxts [(x1, t1)] in
        let* (f, ctxts', e) = invert_right_S f ctxts1 c t2 in
        Choice.return (f, restore_scope ctxts ctxts', RecvF(x1, t1, c, e))
    | STRecvS(t1, t2) ->
        let f, c1 = fresh_chan f in
        let ctxts1 = append_bindings_delta ctxts [(c1, t1)] in
        let* (f, ctxts', e) = invert_right_S f ctxts1 c t2 in
        let c1_consumed = List.assoc_opt c1 ctxts'.d = None in
        let* () = Choice.guard c1_consumed in
        Choice.return (f, ctxts', RecvS(c1, t1, c, e))
    | STExtChoice(labelsesslist) ->
        let synth_branch (l, s) =
            let* (_, ctxts', eP) = invert_right_S f ctxts c s in
            Choice.return (l, (ctxts', eP))
        in
        let* branches = ChoiceUtils.map_list synth_branch labelsesslist in
        let ctxtsl = List.map (fun (_, (ctxts', _)) -> ctxts') branches in
        let* ctxts' = ChoiceUtils.of_option (deltas_are_equal ctxtsl) in
        let labelproclist = List.map (fun (l, (_, e)) -> (l, e)) branches in
        Choice.return (f, restore_scope ctxts ctxts', Choice(c, labelproclist))
    | STRec(k, x, t) ->
        if k <= 0 then
            let* (f, ctxts, eWander) = wander f ctxts c goal None in
            Choice.mplus
                (
                (* wander + fwd *)
                let synthFwdCombination = fun (c', _) -> synth_fwd f ctxts c' c goal in
                let* (f, ctxts', eFwd) = ChoiceUtils.map_mplus_list synthFwdCombination (get_sync_bindings is_tyS_left_async ctxts.d) in
                let eWanderAndFwd = subst_continuation_exp eWander eFwd in
                Choice.return (f, ctxts', eWanderAndFwd)
                )
                (
                (* wander + spawn + fwd *)
                let* tRecLam = ChoiceUtils.of_option (List.assoc_opt ctxts.xRecLam ctxts.p) in
                let tProcess = get_return_type tRecLam in
                let* (f, ctxts', eSpawn) = focus_left_TProcess f ctxts c goal (Some tProcess) in
                let* cSpawn = ChoiceUtils.of_option (get_Spawn_c eSpawn) in
                let* (f, ctxts'', eFwd) = synth_fwd f ctxts' cSpawn c goal in
                let eSpawnAndFwd = subst_continuation_exp eSpawn eFwd in
                let eWanderAndSpawn = subst_continuation_exp eWander eSpawnAndFwd in
                Choice.return (f, ctxts'', eWanderAndSpawn)
                )
        else
            let* tUnfolded = ChoiceUtils.of_option (unfold t (STRec(k-1, x, t)) x) in
            invert_right_S f ctxts c tUnfolded
    | STDeclr(x) ->
        invert_right_S f ctxts c (lookup_declr ctxts.g x)
    | _ -> invert_left_S f ctxts c goal

and invert_left_F f ctxts goal =
    let* f = increment_depth f in
    debugF f ctxts goal goal "invertLeftF";
    match extract_first_async is_tyF_left_async ctxts.p with
    | Some ((x, t), p') ->
        let ctxts_in = ctxts in
        let ctxts = { ctxts with p = p' } in
        begin match t with
        | TConstructor _ ->
            let synth_constructor_branch (x_c, tF) =
                (* instantiate the scheme and match its result against the scrutinee *)
                let* (f, ctxts', args', subst) = ChoiceUtils.of_option (instantiate_constructor f ctxts tF t) in
                let goal' = unify_subst_tyF subst goal in
                (* the pattern variables, one per instantiated argument *)
                let f, rev_x_args =
                    List.fold_left (fun (f, acc) _ ->
                        let f, xi = fresh_id f in (f, xi::acc)) (f, []) args'
                in
                let x_args = List.rev rev_x_args in
                let ctxts'' = append_bindings_psi ctxts' (List.combine x_args args') in
                (* inversion continues, so a nested datatype argument is matched in
                   turn and decide sees the pattern variables *)
                let* (_, ctxts''', e_branch) = invert_left_F f ctxts'' goal' in
                Choice.return (ctxts''', x_c, x_args, e_branch)
            in
            let x_constructors = constructors_of ctxts.c t in
            let* () = Choice.guard (not (List.is_empty x_constructors)) in
            let* branches = ChoiceUtils.map_list synth_constructor_branch x_constructors in
            let ctxtsl = List.map (fun (ctxts', _, _, _) -> ctxts') branches in
            let* ctxts' = ChoiceUtils.of_option (deltas_are_equal ctxtsl) in
            let branches = List.map (fun (_, x_c, x_args, e_branch) -> (x_c, x_args, e_branch)) branches in
            Choice.return (f, restore_scope ctxts_in ctxts', Match(Var(x), branches))
        | _ -> Choice.fail
        end
    | None -> focus_decide_F f ctxts goal

and invert_left_S f ctxts c goal =
    let* f = increment_depth f in
    debugS f ctxts goal goal "invertLeftS";
    match extract_first_async is_tyS_left_async ctxts.d with
    | Some ((c', t'), d') ->
        let ctxts = { ctxts with d = d' } in
        begin match t' with
        | STSendF(t1, t2) ->
            let f, x = fresh_id f in
            let ctxts1 = append_bindings_psi ctxts [(x, t1)] in
            let ctxts2 = append_bindings_delta ctxts1 [(c', t2)] in
            let* (f, ctxts', e) = invert_left_S f ctxts2 c goal in
            let c'_consumed = List.assoc_opt c' ctxts'.d = None in
            let* () = Choice.guard c'_consumed in
            Choice.return (f, restore_scope ctxts ctxts', RecvF(x, t1, c', e))
        | STSendS(t1, t2) ->
            let f, c1 = fresh_chan f in
            let ctxts1 = append_bindings_delta ctxts [(c1, t1); (c', t2)] in
            let* (f, ctxts', e) = invert_left_S f ctxts1 c goal in
            let channels_consumed =
                List.assoc_opt c1 ctxts'.d = None && List.assoc_opt c' ctxts'.d = None
            in
            let* () = Choice.guard channels_consumed in
            Choice.return (f, ctxts', RecvS(c1, t1, c', e))
        | STUnit -> 
            let* (f, ctxts', e) = invert_left_S f ctxts c goal in
            Choice.return (f, ctxts', Wait(c', e))
        | STIntChoice(labelsesslist) ->
            let synth_branch (l, s) =
                let ctxtsn = append_bindings_delta ctxts [(c', s)] in
                let* (_, ctxts', eP) = invert_left_S f ctxtsn c goal in
                let c'_consumed = List.assoc_opt c' ctxts'.d = None in
                let* () = Choice.guard c'_consumed in
                Choice.return (l, (ctxts', eP))
            in
            let* branches = ChoiceUtils.map_list synth_branch labelsesslist in
            let ctxtsl = List.map (fun (_, (ctxts', _)) -> ctxts') branches in
            let* ctxts' = ChoiceUtils.of_option (deltas_are_equal ctxtsl) in
            let labelproclist = List.map (fun (l, (_, e)) -> (l, e)) branches in
            Choice.return (f, restore_scope ctxts ctxts', Choice(c', labelproclist))
        | _ -> Choice.fail
        end
    | None -> focus_decide_S f ctxts c goal

and focus_decide_F f ctxts goal =
    debugF f ctxts goal goal "focusDecideF";
    let left_focuses = List.map (fun (xFoc, tFoc) -> focus_left_F f ctxts (Var xFoc) tFoc goal) (get_sync_bindings is_tyF_left_async ctxts.p) in
    let right_focus = focus_right_F f ctxts goal in
    ChoiceUtils.mplus_list (left_focuses @ [right_focus])

and focus_decide_S f ctxts c goal =
    debugS f ctxts goal goal "focusDecideS";
    let left_focuses = List.map (fun (xFoc, tFoc) -> focus_left_S f ctxts xFoc tFoc c goal) (get_sync_bindings is_tyS_left_async ctxts.d) in
    let right_focus = focus_right_S f ctxts c goal in
    let wander_focus =
        let* (f, ctxts', eWander) = wander f ctxts c goal (Some (TProcess([], goal))) in
        let* (f, ctxts'', eFwd) =
            ChoiceUtils.map_mplus_list (fun (c', _) -> synth_fwd f ctxts' c' c goal) ctxts'.d in
        Choice.return (f, ctxts'', subst_continuation_exp eWander eFwd)
    in
    ChoiceUtils.mplus_list (left_focuses @ [wander_focus; right_focus])

and focus_right_F f ctxts goal =
    let* f = increment_depth f in
    debugF f ctxts goal goal "focusRightF";
    match goal with
    | TAtomic(TInt) -> Choice.return (f, ctxts, Int(1))
    | TAtomic(TBool) -> Choice.return (f, ctxts, Bool(true))
    (* nothing builds a value of a type chosen elsewhere -- one can only come
       from Δ or Ψ, which is left focus's business, not right's *)
    | TAtomic(TRigidVar _) -> Choice.fail
    (* synth refuses a free one, and the head of a spine settles the rest before
       any argument is searched, so neither sort of variable reaches this *)
    | TAtomic(TPolyVar _) -> Choice.fail
    | TConstructor _ ->
        let synth_constructor_select (x_c, tF) = 
            (* instantiate the scheme and match its result against the goal *)
            let* (f, ctxts', args', _) = ChoiceUtils.of_option (instantiate_constructor f ctxts tF goal) in
            (* each argument is its own subgoal, so it is decided rather than kept
               in right focus, which only a literal could answer *)
            let rec synth_args f ctxts acc args =
                match args with
                | [] -> Choice.return (f, ctxts, List.rev acc)
                | a::args' ->
                    let* (f, ctxts, e) = focus_decide_F f ctxts a in
                    synth_args f ctxts (e::acc) args'
            in
            let* (f, ctxts, e_args) = synth_args f ctxts' [] args' in
            Choice.return (f, ctxts, Constructor(x_c, e_args))
        in
        (* a spent budget leaves the datatype with no values at all *)
        if budget_of goal <= 0 then Choice.fail
        else ChoiceUtils.map_mplus_list synth_constructor_select (constructors_of ctxts.c goal)
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
    | STRecVar _ -> Choice.fail
    | _ -> invert_right_S f ctxts c goal (* goal is not right sync, therefore switch back to inversion phase *)

(** Left focus on the binding whose type is [tFocus].
    [eFocus] is the application spine built so far: callers start it at
    [Var x] for the binding [x] being focused, and each arrow rule extends it
    with the argument it synthesizes, so the term is assembled on the way down. *)
and focus_left_F f ctxts eFocus tFocus goal =
    let* f = increment_depth f in
    debugF f ctxts goal tFocus "focusLeftF";
    match tFocus with
    | TArrow(t1, t2) ->
        let* () = Choice.guard (return_type_may_match_goal tFocus goal) in
        let* (f, ctxts', e1) = invert_right_F f ctxts t1 in
        focus_left_F f ctxts' (App(eFocus, e1)) t2 goal
    | TProcess _ | TAtomic _ ->
        (* the spine is finished, and answers the goal when it already is the goal *)
        if tyF_equiv ctxts.g [] tFocus goal then Choice.return (f, ctxts, eFocus)
        else Choice.fail
    | TConstructor _ ->
        (* a datatype either ends the spine like any other positive type, or --
           being positive -- releases: the spine is bound into Ψ for inversion to
           case-analyse. Both are offered. The record of what has been released
           travels out with the contexts rather than being scoped, since a released
           datatype answers the same goal its own spine was focused at and decide
           would otherwise reach that spine again in every argument it searches,
           nesting releases without a bound; the price is that one release rules
           out its siblings. An inert datatype is skipped, having no branches to
           open. *)
        let terminal =
            if tyF_equiv ctxts.g [] tFocus goal then Choice.return (f, ctxts, eFocus)
            else Choice.fail
        in
        let release =
            let released_already = List.exists (tyF_equiv ctxts.g [] tFocus) ctxts.released in
            if not (is_tyF_left_async tFocus) || released_already then Choice.fail
            else
                let f, x = fresh_id f in
                let ctxts' = append_bindings_psi ctxts [(x, tFocus)] in
                let ctxts' = { ctxts' with released = tFocus :: ctxts'.released } in
                let* (f, ctxts'', e) = invert_left_F f ctxts' goal in
                Choice.return (f, ctxts'', Let(x, eFocus, e))
        in
        Choice.mplus terminal release
    | TRefinement(_, tA, _) ->
        if tyF_equiv ctxts.g [] (TAtomic tA) goal || (is_TRefinement goal && Cvc5adapter.sat !print_debug ctxts.p tFocus goal)
            then Choice.return (f, ctxts, eFocus)
        else Choice.fail
    | TDeclr x ->
        let declr_matches_goal =
            match List.assoc_opt x ctxts.p with
            | Some t -> tyF_equiv ctxts.g [] t goal
            | None -> false
        in
        if tyF_equiv ctxts.g [] tFocus goal || declr_matches_goal then Choice.return (f, ctxts, eFocus)
        else Choice.fail
    | TForAll _ ->
        (* The return type is where the goal reaches the scheme, so unifying it
           settles every variable the goal determines before an argument is
           searched at one; unifying the whole scheme would compare an arrow
           against the goal and never match. As in focus_right_F, the catch
           covers only the eager part. *)
        let instantiated =
            try
                let f, t_inst = instantiate_tyF f tFocus in
                let subst = unify ctxts.g (get_return_type t_inst) goal in
                Some (f, unify_subst_tyF subst t_inst, unify_subst_tyF subst goal,
                      unify_subst_ctxts subst ctxts)
            with Fail _ -> None
        in
        let* (f, t_inst', goal', ctxts') = ChoiceUtils.of_option instantiated in
        (* What the return type left undetermined is chosen here rather than
           where it is used, so one substitution reaches the whole spine:
           two argument positions sharing a variable are searched at the same type, 
           and so a mismatched pair is never built, rather than built and rejected. *)
        let* subst = Choice.of_list (ground_substitutions t_inst') in
        focus_left_F f (unify_subst_ctxts subst ctxts') eFocus (unify_subst_tyF subst t_inst') (unify_subst_tyF subst goal')


and focus_left_S f ctxts cFocus tFocus c goal =
    let* f = increment_depth f in
    debugS f ctxts goal tFocus "focusLeftS";
    match tFocus with
    | STRecvF(t1, t2) ->
        let* (ctxts0, _) = ChoiceUtils.of_option (consume_channel ctxts cFocus) in
        let ctxts1 = append_bindings_delta ctxts0 [(cFocus, t2)] in
        let* (f, ctxts', e1) = invert_right_F f ctxts1 t1 in
        let* (f, ctxts'', e2) = focus_left_S f ctxts' cFocus t2 c goal in
        Choice.return (f, ctxts'', SendF(cFocus, e1, e2))
    | STRecvS(t1, t2) -> 
        let f, c' = fresh_chan f in
        let* (ctxts0, _) = ChoiceUtils.of_option (consume_channel ctxts cFocus) in
        let ctxts1 = append_bindings_delta ctxts0 [(cFocus, t2)] in
        let* (f, ctxts', e1) = invert_right_S f ctxts1 c' t1 in
        let* (f, ctxts'', e2) = focus_left_S f ctxts' cFocus t2 c goal in
        Choice.return (f, ctxts'', SendS(cFocus, c', e1, e2))
    | STExtChoice(labelsesslist) -> 
        let synth_choice_select (l, s) =
            let* (ctxts0, _) = ChoiceUtils.of_option (consume_channel ctxts cFocus) in
            let ctxts1 = append_bindings_delta ctxts0 [(cFocus, s)] in
            let* (f', ctxts', e1) = focus_left_S f ctxts1 cFocus s c goal in
            Choice.return (f', ctxts', ChoiceSelect(cFocus, l, e1))
        in
        ChoiceUtils.map_mplus_list synth_choice_select labelsesslist
    | STRec(k, x, t) ->
        if k <= 0 then
            Choice.return (f, ctxts, Hole(c, goal))
        else
            let* tUnfolded = ChoiceUtils.of_option (unfold t (STRec(k-1, x, t)) x) in
            let* (ctxts0, _) = ChoiceUtils.of_option (consume_channel ctxts cFocus) in
            let ctxts1 = append_bindings_delta ctxts0 [(cFocus, tUnfolded)] in
            focus_left_S f ctxts1 cFocus tUnfolded c goal
    | STDeclr(x) ->
        let tDeclr = lookup_declr ctxts.g x in
        let* (ctxts0, _) = ChoiceUtils.of_option (consume_channel ctxts cFocus) in
        let ctxts1 = append_bindings_delta ctxts0 [(cFocus, tDeclr)] in
        focus_left_S f ctxts1 cFocus tDeclr c goal
    | _ ->
        let* () = Choice.guard (is_tyS_left_async tFocus) in
        invert_left_S f ctxts c goal

(* wandering *)

(** Wandering injects alternative expressions which don't directly follow the original
    type, unlocking more varied solutions.
    Each produced expression leaves a [Hole] at the continuation, for the caller to plug. 
    [spawn_filter] bounds what types of TProcesses may be spawned. *)
and wander f ctxts c goal spawn_filter =
    debugS f ctxts goal goal "wander";
    let skipChoice = Choice.return (f, ctxts, Hole(c, goal)) in
    let spawnChoice = focus_left_TProcess f ctxts c goal spawn_filter in
    let unfoldChoice = wander_unfold f ctxts c goal in
    ChoiceUtils.mplus_list [skipChoice; spawnChoice; unfoldChoice]

and wander_unfold f ctxts c goal =
    let recsessl = List.filter (fun (_, t) -> is_STRec (resolve_declr ctxts.g t)) (get_sync_bindings is_tyS_left_async ctxts.d) in
    let synth_unfolds (c', t') = focus_left_S f ctxts c' t' c goal in
    ChoiceUtils.map_mplus_list synth_unfolds recsessl

(* reusable synthesis functions *)

(**
synthesizes possible spawn expressions which output a desired session-type, containing a placeholder continuation expression
@param goal: the session type the continuation still has to offer on [c], recorded in the placeholder it leaves behind
@param goal_tProcess_filter: option possibly containing a TProcess(insl, outs) whose outs serves as a filter for valid spawnable processes with equivalent outs session-types, which will be provided by the spawned channel. If goal_tProcess_filter is None, then any TProcess(_, _) is spawnable
*)
and focus_left_TProcess f ctxts c goal goal_tProcess_filter =
    let* f = increment_depth f in
    let f, cSpawn = fresh_chan f in
    (* an opened binding is spawnable when the process it ends at offers an
       equivalent session (when a filter is given) *)
    let offers_goal_session tReturn =
        match goal_tProcess_filter with
        | None -> true
        | Some filter_t ->
            match get_TProcess_outs tReturn, get_TProcess_outs filter_t with
            | Some outs, Some filter_outs -> tyS_equiv ctxts.g [] outs filter_outs
            | _ -> false
    in
    (* A scheme is instantiated, matched against the filter, and its leftovers grounded,
       in the order left focus on a scheme uses. What comes back is a concrete monomorphic 
       instantiation, which can be used like any other existing spawnable type *)
    let instantiate_binding (x, t) =
        match t with
        | TForAll _ ->
            let matched =
                try
                    let f, tInst = instantiate_tyF f t in
                    let subst =
                        match get_TProcess_outs (get_return_type tInst), Option.bind goal_tProcess_filter get_TProcess_outs with
                        | Some outs, Some filter_outs -> unify_tyS ctxts.g outs filter_outs
                        | _ -> []
                    in
                    Some (f, unify_subst_tyF subst tInst)
                with Fail _ -> None
            in
            let* (f, tInst) = ChoiceUtils.of_option matched in
            let* subst = Choice.of_list (ground_substitutions tInst) in
            Choice.return (f, x, unify_subst_tyF subst tInst)
        | _ -> Choice.return (f, x, t)
    in
    let spawnable_proc_list =
        List.filter (fun (_, t) -> offers_TProcess t) (get_sync_bindings is_tyF_left_async ctxts.p)
    in
    let* (f, x, t) = ChoiceUtils.map_mplus_list instantiate_binding spawnable_proc_list in
    let tProcess = get_return_type t in
    let* () = Choice.guard (offers_goal_session tProcess) in
    let* insl = ChoiceUtils.of_option (get_TProcess_insl tProcess) in
    let* outs = ChoiceUtils.of_option (get_TProcess_outs tProcess) in
    let* (f, ctxts', eApp) = focus_left_F f ctxts (Var x) t tProcess in
    let* (ctxts'', incl) = consume_channels_by_tyS ctxts' insl in
    let ctxts''' = append_bindings_delta ctxts'' [(cSpawn, outs)] in
    Choice.return (f, ctxts''', Spawn(cSpawn, eApp, incl, Hole(c, goal)))

and synth_fwd f ctxts cToFwd c goal =
    let* (ctxts', tToFwd) = ChoiceUtils.of_option (consume_channel ctxts cToFwd) in
    let* () = Choice.guard (tyS_equiv ctxts.g [] tToFwd goal) in
    let* () = Choice.guard (delta_is_empty ctxts') in
    Choice.return (f, ctxts', Fwd(cToFwd, c, goal))

(* entry point *)

(** How the caller wants to be involved in choosing between solutions.
    [Auto] takes the first one the search produces and never touches stdin, which
    is what a script or a test harness needs; [Interactive] prints them all and
    lets the user narrow and pick.

    Either way the choice happens *after* the search, over whole programs. It
    used to happen inside the external choice rule, one label at a time, which
    committed the search to a combination of branch bodies it could never
    backtrack into. *)
type synth_mode = Auto | Interactive

let mode = ref Interactive
let set_mode m = mode := m

let mode_of_string s =
    match String.lowercase_ascii s with
    | "auto" -> Some Auto
    | "interactive" -> Some Interactive
    | _ -> None

let env_flag name =
    match Sys.getenv_opt name with
    | None | Some ("0" | "false" | "FALSE" | "no" | "NO") -> false
    | Some _ -> true

(** What the search spent, on stderr so it stays out of the solution output the
    golden harness pins. Reported per hole, since [synth] is called once per hole. *)
let print_stats () =
    Printf.eprintf "sessynth stats: steps=%d depth_cutoffs=%d solver_calls=%d\n%!"
        !Flags.steps !Flags.depth_cutoffs !Flags.solver_calls

let synth n_sol g p c d goal =
    set_print_debug (env_flag "SESSYNTH_DEBUG");
    let stats = env_flag "SESSYNTH_STATS" in
    Flags.reset_stats ();
    if n_sol < 1 then raise (Fail ("a hole must ask for at least one solution, not " ^ string_of_int n_sol));
    let reject_duplicates where bindings =
        match find_first_duplicate_name bindings with
        | Some x -> raise (Fail ("channel " ^ x ^ " is bound more than once in " ^ where))
        | None -> ()
    in
    let reject_unbound_recvar where unbound =
        match unbound with
        | Some x -> raise (Fail ("recursion variable " ^ x ^ " in " ^ where ^ " is not bound by any enclosing rec"))
        | None -> ()
    in
    reject_duplicates "the linear context" d;
    (match find_first_duplicate_name c with
     | Some x -> raise (Fail ("constructor " ^ x ^ " is declared more than once"))
     | None -> ());
    List.iter (fun binding ->
        match ill_formed_constructor binding with
        | Some d -> raise (Fail ("malformed constructor: " ^ d))
        | None -> ()) c;
    (match goal with
     | TProcess(incsl, _) -> reject_duplicates "the goal's input channels" incsl
     | _ -> ());
    let binders = refinement_binders goal in
    let clash =
        match find_first_duplicate_name (List.map (fun x -> (x, ())) binders) with
        | Some _ as dup -> dup
        | None -> List.find_opt (fun x -> List.mem_assoc x p) binders
    in
    (match clash with
     | Some x -> raise (Fail ("refinement variable " ^ x ^ " in the goal is bound more than once"))
     | None -> ());
    reject_unbound_recvar "the goal" (unbound_recvar_tyF goal);
    List.iter (fun (x, t) -> reject_unbound_recvar ("the type of " ^ x) (unbound_recvar_tyF t)) p;
    List.iter (fun (x, t) -> reject_unbound_recvar ("the constructor " ^ x) (unbound_recvar_tyF t)) c;
    List.iter (fun (c, s) -> reject_unbound_recvar ("the type of channel " ^ c) (unbound_recvar_tyS [] s)) d;
    (* a Γ definition has to be closed for [resolve_declr] to be capture-free *)
    List.iter (fun (x, s) -> reject_unbound_recvar ("the declaration of " ^ x) (unbound_recvar_tyS [] s)) g;
    let reject_unbound_polyvar where unbound =
        match unbound with
        | Some a -> raise (Fail ("type variable " ^ a ^ " in " ^ where ^ " is not bound by any forall"))
        | None -> ()
    in

    reject_unbound_polyvar "the goal" (unbound_polyvar_tyF goal);
    List.iter (fun (x, t) -> reject_unbound_polyvar ("the type of " ^ x) (unbound_polyvar_tyF t)) p;
    List.iter (fun (x, t) -> reject_unbound_polyvar ("the constructor " ^ x) (unbound_polyvar_tyF t)) c;
    List.iter (fun (c, s) -> reject_unbound_polyvar ("the type of channel " ^ c) (unbound_polyvar_tyS s)) d;
    List.iter (fun (x, s) -> reject_unbound_polyvar ("the declaration of " ^ x) (unbound_polyvar_tyS s)) g;
    let reject_ill_formed where defect =
        match defect with
        | Some d -> raise (Fail ("malformed type: " ^ d ^ " in " ^ where))
        | None -> ()
    in
    reject_ill_formed "the goal" (ill_formed_scheme_tyF goal);
    List.iter (fun (x, t) -> reject_ill_formed ("the type of " ^ x) (ill_formed_scheme_tyF t)) p;
    List.iter (fun (x, t) -> reject_ill_formed ("the constructor " ^ x) (ill_formed_scheme_tyF t)) c;
    List.iter (fun (c, s) -> reject_ill_formed ("the type of channel " ^ c) (ill_formed_scheme_tyS s)) d;
    List.iter (fun (x, s) -> reject_ill_formed ("the declaration of " ^ x) (ill_formed_scheme_tyS s)) g;
    (* after the shape checks above, so constructors_of has the bindings it expects *)
    (match constructor_arity_disagreement c with
     | Some d -> raise (Fail d)
     | None -> ());
    let reject_use where defect =
        match defect with
        | Some d -> raise (Fail (d ^ ", in " ^ where))
        | None -> ()
    in
    reject_use "the goal" (datatype_use_defect_tyF c goal);
    List.iter (fun (x, t) -> reject_use ("the type of " ^ x) (datatype_use_defect_tyF c t)) p;
    List.iter (fun (x, t) -> reject_use ("the constructor " ^ x) (datatype_use_defect_tyF c t)) c;
    List.iter (fun (x, s) -> reject_use ("the type of channel " ^ x) (datatype_use_defect_tyS c s)) d;
    List.iter (fun (x, s) -> reject_use ("the declaration of " ^ x) (datatype_use_defect_tyS c s)) g;
    (match cyclic_declr g with
     | Some x -> raise (Fail ("session type " ^ x ^ " is defined in terms of itself; use rec instead"))
     | None -> ());


    let f, ctxts = initialize_flags (), initialize_ctxts in
    let ctxts = append_bindings_gamma ctxts g in
    let ctxts = append_bindings_psi ctxts p in
    let ctxts = append_bindings_constructors ctxts c in
    let ctxts = append_bindings_delta ctxts d in
    let solutions_choice =
        let* (_, ctxts', expF') = invert_right_F f ctxts goal in
        let* () = Choice.guard (delta_is_empty ctxts') in
        let* () = Choice.guard (not (has_hole_expF expF')) in
        Choice.return expF'
    in
    let expl = ChoiceUtils.run_n_distinct n_sol solutions_choice in
    (* before the empty check, since an exhausted search is the interesting one to measure *)
    if stats then print_stats ();
    if List.is_empty expl then raise (Fail "No valid expression for the provided type") else
    print_newline ();
    print_solutions expl;
    match !mode with
    | Auto -> List.hd expl
    | Interactive ->
        if List.length expl = 1 then List.hd expl
        else
            let expl = interactive_filter_solutions expl in
            let rec choose_exp () =
                print_string "Select solution: ";
                match read_line_opt () with
                | None -> print_endline "\nno input, taking solution 0"; List.hd expl
                | Some line ->
                    match int_of_string_opt (String.trim line) with
                    | Some i when i >= 0 && i < List.length expl -> List.nth expl i
                    | _ -> print_endline "Invalid choice.\n"; choose_exp ()
            in choose_exp ()

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