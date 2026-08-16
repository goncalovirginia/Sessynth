open Language
open Printer
open TyUtils
open Contexts
open Polymorphism
open Adts
open Flags
open ChoiceUtils.Let_syntax

module Language = Language

(* re-exported so that Sessynth.Fail keeps naming the same exception *)
exception Fail = TyUtils.Fail

(* debug tracing *)

let debugF f ctxts goal tFocus curr_fun =
    if not f.printDebug then ()
    else let indent = String.make (f.usedFuel * 2) ' ' in
        print_endline (indent ^ "usedFuel: " ^ string_of_int f.usedFuel); 
        match curr_fun with
        | "invertRightF" | "invertLeftF" | "focusDecideF" ->
            print_endline (indent ^ curr_fun ^ ": " ^ tyF_to_string goal);
            print_endline (indent ^ "p: " ^ psi_to_string ctxts.p)
        | _ ->
            print_endline (indent ^ curr_fun ^ ": " ^ tyF_to_string goal);
            print_endline (indent ^ "tFocus: " ^ tyF_to_string tFocus)

let debugS f ctxts goal tFocus curr_fun =
    if not f.printDebug then ()
    else let indent = String.make (f.usedFuel * 2) ' ' in
        print_endline (indent ^ "usedFuel: " ^ string_of_int f.usedFuel); 
        match curr_fun with
        | "invertRightS" | "invertLeftS" | "focusDecideS" ->
            print_endline (indent ^ curr_fun ^ ": " ^ tyS_to_string goal);
            print_string (indent ^ "d: " ^ delta_to_string ctxts.d)
        | _ ->
            print_endline (indent ^ curr_fun ^ ": " ^ tyS_to_string goal);
            print_endline (indent ^ "tFocus: " ^ tyS_to_string tFocus)

let rec subst e1 x e2 =
    match e1 with 
    | Var y -> if x=y then e2 else e1 
    | App(e, e') -> App (subst e x e2 , subst e' x e2)
    | Lam(y, t, e) -> if x <> y then Lam (y,t,(subst e x e2)) else e1 
    | _ -> raise (Fail("subst pattern matching not defined for " ^ expF_to_string e1 0))

let get_keys kvl =
    List.map (fun (k, v) -> k) kvl

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
    | Spawn(c, _, _, _) -> Some c
    | _ -> None

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

(* inversion and focusing *)

(* flags + gamma-async; gamma-sync; psi-async; psi-async; delta-async; delta-sync |- P :: c : goal *)

let rec invert_right_F f ctxts goal =
    let* f = consume_fuel f in
    debugF f ctxts goal goal "invertRightF";
    match goal with
    | TArrow(t1, t2) ->
        let f, x = match t1 with
            | TRefinement(x, _, _) -> f, x
            | _ -> fresh_id f in 
        let ctxts1 = append_bindings_psi ctxts [(x, t1)] in
        begin match get_return_type t2 with
        | TProcess(_, STRec _) when not (List.mem_assoc f.xRecLam ctxts.p) ->
            let f, ctxts2 =
            match find_binding_for_tyF ctxts goal with
            | Some xRecFun ->
                let f = { f with xRecLam = xRecFun } in
                f, ctxts1
            | None ->
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
            let f =
                if f.xRecLam <> "" then f
                else match find_binding_for_tyF ctxts goal with
                    | Some xRecFun -> { f with xRecLam = xRecFun }
                    | None -> f
            in
            let ctxts1 = append_bindings_delta ctxts incsl in
            let f, c = fresh_chan f in
            let* (f, ctxts', e) = invert_right_S f ctxts1 c outs in
            let incsl_consumed = List.for_all (fun (c', _) -> List.assoc_opt c' ctxts'.d = None) incsl in
            let* () = Choice.guard incsl_consumed in
            Choice.return (f, ctxts', Process(c, e, outs, incsl))
    | TDeclr(x) ->
        let* t = ChoiceUtils.of_option (List.assoc_opt x ctxts.p) in
        invert_right_F f ctxts t
    | TRefinement(x, t1, t2) ->
        (* an infeasible or unparseable SyGuS goal fails this branch rather than aborting the whole search *)
        let solution = 
            try Some (Cvc5adapter.solve ctxts.p goal) 
            with Cvc5adapter.CVC5Infeasible _ | Cvc5adapter.CVC5ParseError _ | Cvc5adapter.CVC5Error _ -> None
        in
        let* solution = ChoiceUtils.of_option solution in
        Choice.return (f, ctxts, solution)
    | TForAll(xkl, t) -> 
        let f, t' = instantiate_tyF f goal in
        invert_right_F f ctxts t' 
    | _ -> invert_left_F f ctxts goal

and invert_right_S f ctxts c goal = 
    let* f = consume_fuel f in
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
        let c1_consumed = List.assoc_opt c1 ctxts'.d = None in
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
        let* (_, (f', ctxts', _)) = ChoiceUtils.of_option (List.nth_opt branches 0) in
        Choice.return (f', ctxts', Choice(c, labelproclist))
    | STRec(k, x, t) ->
        if k <= 0 then
            let* (f, ctxts, eWander) = wander f ctxts c goal in
            Choice.mplus
                (
                (* wander + fwd *)
                let synthFwdCombination = fun (c', t') -> synth_fwd f ctxts c' c t' in
                let* (f, ctxts', eFwd) = ChoiceUtils.map_mplus_list synthFwdCombination (get_sync_bindings is_tyS_left_async ctxts.d) in
                let eWanderAndFwd = subst_continuation_exp eWander eFwd in
                Choice.return (f, ctxts', eWanderAndFwd)
                )
                (
                (* wander + spawn + fwd *)
                let* tRecLam = ChoiceUtils.of_option (List.assoc_opt f.xRecLam ctxts.p) in
                let tProcess = get_return_type tRecLam in
                let* (f, ctxts', eSpawn) = focus_left_TProcess f ctxts c (Some tProcess) in
                let* cSpawn = ChoiceUtils.of_option (get_Spawn_c eSpawn) in
                let* (f, ctxts'', eFwd) = synth_fwd f ctxts' cSpawn c t in
                let eSpawnAndFwd = subst_continuation_exp eSpawn eFwd in
                let eWanderAndSpawn = subst_continuation_exp eWander eSpawnAndFwd in
                Choice.return (f, ctxts'', eWanderAndSpawn)
                )
        else
            let* tUnfolded = ChoiceUtils.of_option (unfold t (STRec(k-1, x, t)) x) in
            invert_right_S f ctxts c tUnfolded
    | STDeclr(x) ->
        (* TODO: session-type declarations belong in Γ, not Δ (see planned task 2) *)
        let* t = ChoiceUtils.of_option (List.assoc_opt x ctxts.d) in
        invert_right_S f ctxts c t
    | _ -> invert_left_S f ctxts c goal

and invert_left_F f ctxts goal =
    let* f = consume_fuel f in
    debugF f ctxts goal goal "invertLeftF";
    match extract_first_async is_tyF_left_async ctxts.p with
    | Some ((x, t), p') ->
        let ctxts = { ctxts with p = p' } in
        begin match t with
        | TConstructor(x, args) ->
            let synth_constructor_branch (x_c, tF) =
                (* instantiate the scheme and match its result against the scrutinee *)
                let* (f, ctxts', args', subst) = ChoiceUtils.of_option (instantiate_constructor f ctxts tF t) in
                let goal' = unify_subst_tyF subst goal in
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
            let x_constructors = constructors_of ctxts.c t in
            let* () = Choice.guard (not (List.is_empty x_constructors)) in
            let* branches = ChoiceUtils.map_list synth_constructor_branch x_constructors in
            let ctxtsl = List.map (fun (_, ctxts', _, _, _) -> ctxts') branches in
            let* ctxts' = ChoiceUtils.of_option (deltas_are_equal ctxtsl) in
            let branches = List.map (fun (_, _, x_c, x_args, e_branch) -> (x_c, x_args, e_branch)) branches in
            Choice.return (f, ctxts', Match(Var(x), branches))
        | _ -> Choice.fail
        end
    | None -> focus_decide_F f ctxts goal

and invert_left_S f ctxts c goal =
    let* f = consume_fuel f in
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
            Choice.return (f, ctxts', RecvF(x, t1, c', e))
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
            let synth_branch f (l, s) =
                let f, cn = fresh_chan f in
                let ctxtsn = append_bindings_delta ctxts [(cn, s)] in
                let* (f', ctxts', eP) = invert_left_S f ctxtsn c goal in
                let cn_consumed = List.assoc_opt cn ctxts'.d = None in
                let* () = Choice.guard cn_consumed in
                Choice.return (f', (l, (f', ctxts', eP)))
            in
            let* (_, branches) = ChoiceUtils.map_list_state f synth_branch labelsesslist in
            let ctxtsl = List.map (fun (_, (_, ctxts', _)) -> ctxts') branches in
            let* ctxts' = ChoiceUtils.of_option (deltas_are_equal ctxtsl) in
            let labelproclist = List.map (fun (l, (_, _, e)) -> (l, e)) branches in
            Choice.return (f, ctxts', Choice(c', labelproclist))
        | _ -> Choice.fail
        end
    | None -> focus_decide_S f ctxts c goal

and focus_decide_F f ctxts goal =
    debugF f ctxts goal goal "focusDecideF";
    let left_focuses = List.map (fun (xFoc, tFoc) -> focus_left_F f ctxts xFoc tFoc goal) (get_sync_bindings is_tyF_left_async ctxts.p) in
    let right_focus = focus_right_F f ctxts goal in
    ChoiceUtils.mplus_list (left_focuses @ [right_focus])

and focus_decide_S f ctxts c goal =
    debugS f ctxts goal goal "focusDecideS";
    let left_focuses = List.map (fun (xFoc, tFoc) -> focus_left_S f ctxts xFoc tFoc c goal) (get_sync_bindings is_tyS_left_async ctxts.d) in
    let right_focus = focus_right_S f ctxts c goal in
    ChoiceUtils.mplus_list (left_focuses @ [right_focus])

and focus_right_F f ctxts goal =
    let* f = consume_fuel f in
    debugF f ctxts goal goal "focusRightF";
    match goal with
    | TAtomic(TInt) -> Choice.return (f, ctxts, Int(1))
    | TAtomic(TBool) -> Choice.return (f, ctxts, Bool(true))
    | TAtomic(TPolyVar(_)) ->
        let ground_atomic_types = [TInt; TBool] in
        let map_unify = fun tA ->
            (* the try covers only the eager unification; the recursive call is
               bound outside it, since a try around a let* would not protect it *)
            let unified =
                try
                    let candidate = TAtomic(tA) in
                    let subst = unify goal candidate in
                    Some (unify_subst_ctxts subst ctxts, unify_subst_tyF subst goal)
                with Fail _ -> None
            in
            let* (ctxts', goal') = ChoiceUtils.of_option unified in
            focus_right_F f ctxts' goal'
        in
        ChoiceUtils.map_mplus_list map_unify ground_atomic_types
    | TConstructor(x, args) ->
        let synth_constructor_select (x_c, tF) = 
            (* instantiate the scheme and match its result against the goal *)
            let* (f, ctxts', args', _) = ChoiceUtils.of_option (instantiate_constructor f ctxts tF goal) in
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
        let x_constructors = constructors_of ctxts.c goal in
        ChoiceUtils.map_mplus_list synth_constructor_select x_constructors
    | _ -> invert_right_F f ctxts goal

and focus_right_S f ctxts c goal =
    let* f = consume_fuel f in
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
    let* f = consume_fuel f in
    debugF f ctxts goal tFocus "focusLeftF";
    match tFocus with
    | TArrow(t1, t2) ->
        let f, c' = fresh_chan f in
        let* (f, ctxts', e2) = focus_left_F f ctxts c' t2 goal in
        let* (f, ctxts'', e1) = invert_right_F f ctxts t1 in
        Choice.return (f, ctxts'', subst e2 c' (App(Var(xFocus), e1)))
    | TProcess _ | TAtomic _ ->
        if tyF_equiv tFocus goal then Choice.return (f, ctxts, Var(xFocus))
        else Choice.fail
    | TRefinement(x, tA, tR) ->
        if tyF_equiv (TAtomic tA) goal || (is_TRefinement goal && Cvc5adapter.sat ctxts.p tFocus goal)
            then Choice.return (f, ctxts, Var(xFocus))
        else Choice.fail
    | TDeclr x ->
        let declr_matches_goal =
            match List.assoc_opt x ctxts.p with
            | Some t -> tyF_equiv t goal
            | None -> false
        in
        if tyF_equiv tFocus goal || declr_matches_goal then Choice.return (f, ctxts, Var(xFocus))
        else Choice.fail
    | TForAll(xkl, t) ->
        (* as in focus_right_F: catch around the eager part only *)
        let instantiated =
            try
                let f, t_inst = instantiate_tyF f tFocus in
                let subst = unify t_inst goal in
                Some (f, unify_subst_tyF subst t_inst, unify_subst_tyF subst goal,
                      unify_subst_ctxts subst ctxts)
            with Fail _ -> None
        in
        let* (f, t_inst', goal', ctxts') = ChoiceUtils.of_option instantiated in
        focus_left_F f ctxts' xFocus t_inst' goal'
    | _ -> invert_left_F f ctxts goal

and focus_left_S f ctxts cFocus tFocus c goal =
    let* f = consume_fuel f in
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
            Choice.return (f, ctxts, Close(""))
        else
            let* tUnfolded = ChoiceUtils.of_option (unfold t (STRec(k-1, x, t)) x) in
            let* (ctxts0, _) = ChoiceUtils.of_option (consume_channel ctxts cFocus) in
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
    let recsessl = List.filter (fun (_, t) -> is_STRec t) (get_sync_bindings is_tyS_left_async ctxts.d) in
    let synth_unfolds (c', t') = focus_left_S f ctxts c' t' c goal in
    ChoiceUtils.map_mplus_list synth_unfolds recsessl

(* reusable synthesis functions *)

(** 
synthesizes possible spawn expressions which output a desired session-type, containing a placeholder continuation expression
@param goal_tProcess_filter: option possibly containing a TProcess(insl, outs) whose outs serves as a filter for valid spawnable processes with equivalent outs session-types, which will be provided by the spawned channel. If goal_tProcess_filter is None, then any TProcess(_, _) is spawnable
*)
and focus_left_TProcess f ctxts c goal_tProcess_filter =
    let* f = consume_fuel f in
    let f, cSpawn = fresh_chan f in
    (* a binding is spawnable when its return type is a process type, and when 
       that process offers an equivalent session (when a filter is given) *)
    let offers_goal_session tReturn =
        match goal_tProcess_filter with
        | None -> true
        | Some filter_t ->
            match get_TProcess_outs tReturn, get_TProcess_outs filter_t with
            | Some outs, Some filter_outs -> tyS_equiv outs filter_outs
            | _ -> false
    in
    let spawnable_proc_list =
        List.filter (fun (_, t) ->
            let tReturn = get_return_type t in
            is_TProcess tReturn && offers_goal_session tReturn
        ) (get_sync_bindings is_tyF_left_async ctxts.p)
    in
    let* (x, t) = Choice.of_list spawnable_proc_list in
    let tProcess = get_return_type t in
    let* insl = ChoiceUtils.of_option (get_TProcess_insl tProcess) in
    let* outs = ChoiceUtils.of_option (get_TProcess_outs tProcess) in
    let* (f, ctxts', eApp) = focus_left_F f ctxts x t tProcess in
    let* (ctxts'', incl) = consume_channels_by_tyS ctxts' insl in
    let ctxts''' = append_bindings_delta ctxts'' [(cSpawn, outs)] in
    Choice.return (f, ctxts''', Spawn(cSpawn, eApp, incl, Close("")))

and synth_fwd f ctxts cToFwd c t =
    let* (ctxts', _) = ChoiceUtils.of_option (consume_channel ctxts cToFwd) in
    Choice.return (f, ctxts', Fwd(cToFwd, c, t))

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