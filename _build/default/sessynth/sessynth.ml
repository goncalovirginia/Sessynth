module Language = Language
open Language
open ChoiceUtils.Let_syntax

exception Fail of string

(* records *)

type flags = { isUnfoldedRight : bool; isUnfoldedLeft : bool; xRecLam : id; currDepth : int; maxDepth : int; printDebug : bool }
    
type gamma = { a : (id * tyS) list; s :  (id * tyS) list }
    
type psi = { a : (id * tyF) list; s :  (id * tyF) list }
    
type delta = { a : (id * tyS) list; s :  (id * tyS) list }
    
type contexts = { g : gamma; p : psi; d : delta }

(* auxiliary functions *)

let initialize_flags maxDepth printDebug =
    let f : flags = { isUnfoldedRight = false; isUnfoldedLeft = false; xRecLam = ""; currDepth = -1; maxDepth = maxDepth; printDebug = printDebug } in
    f

let initialize_ctxts =
    let g : gamma = { a = []; s = [] } in
    let p : psi = { a = []; s = [] } in
    let d : delta = { a = []; s = [] } in
    let ctxts : contexts = { g = g; p = p; d = d } in
    ctxts

let fresh_id = 
    let unique = ref (-1) in
    fun () -> (incr unique; "_x" ^ (string_of_int !unique))

let fresh_function = 
    let unique = ref (-1) in
    fun () -> (incr unique; "_f" ^ (string_of_int !unique))

let fresh_channel = 
    let unique = ref (-1) in
    fun () -> (incr unique; "_c" ^ (string_of_int !unique))

let uOp_to_string t =
    match t with
    | Not -> "!"
    | Neg -> "-"

let bOp_to_string t =
    match t with
    | And -> " && "
    | Or -> " || "
    | Eq -> " == "
    | Gr -> " > "
    | Lt -> " < "
    | GrE -> " >= "
    | LtE -> " <= "
    | Sum -> " + "
    | Sub -> " - "
    | Mult -> " * "
    | Div -> " / "

let tyA_to_string t =
    match t with
    | TInt -> "int"
    | TBool -> "bool"

let rec tyR_to_string t =
    match t with
    | RTUOp(op, t) -> uOp_to_string op ^ tyR_to_string t
    | RTBOp(op, t1, t2) -> tyR_to_string t1 ^ bOp_to_string op ^ tyR_to_string t2
    | RTInt(v) -> string_of_int v
    | RTBool(v) -> string_of_bool v
    | RTVar(x) -> x

and tyF_to_string t =
    match t with 
    | TAtomic(t) -> tyA_to_string t
    | TRefinement(x, t1, RTBool(true)) -> x ^ ":" ^ tyA_to_string t1
    | TRefinement(x, t1, t2) -> "{" ^ x ^ ":" ^ tyA_to_string t1 ^ " | " ^ tyR_to_string t2 ^ "}"
    | TArrow(t1, t2) -> tyF_to_string t1 ^ " -> " ^ tyF_to_string t2
    | TProcess(incsl, outs) -> "{" ^ cs_list_to_string incsl ^ " |- " ^ tyS_to_string outs ^ "}"
    | TDeclr(x) -> x

and tyS_to_string t =
    match t with 
    | STDeclr(x) -> x
    | STSendF(t1, t2) -> tyF_to_string t1 ^ " ∧ " ^ tyS_to_string t2
    | STRecvF(t1, t2) -> tyF_to_string t1 ^ " ⊃ " ^ tyS_to_string t2
    | STSendS(t1, t2) -> tyS_to_string t1 ^ " ⊗ " ^ tyS_to_string t2
    | STRecvS(t1, t2) -> tyS_to_string t1 ^ " -o " ^ tyS_to_string t2
    | STExtChoice(xtl) -> "&{" ^ label_tyS_list_to_string xtl ^ "}"
    | STIntChoice(xtl) -> "⊕{" ^ label_tyS_list_to_string xtl ^ "}"
    | STRec(x, t) -> "𝜇" ^ x ^ "." ^ tyS_to_string t
    | STRecVar(x) -> x
    | STUnit -> "1"

and cs_list_to_string csl =
    match csl with
    | [] -> ""
    | [(c, s)] -> c ^ ":" ^ tyS_to_string s
    | (c, s)::csl' -> c ^ ":" ^ tyS_to_string s ^ ", " ^ cs_list_to_string csl'

and label_tyS_list_to_string xtl =
    match xtl with
    | [] -> ""
    | [(l, t)] -> l ^ ":" ^ tyS_to_string t
    | (l, t)::xtl' -> l ^ ":" ^ tyS_to_string t ^ ", " ^ label_tyS_list_to_string xtl' 

let rec expF_to_string e =
    match e with 
    | Int(v) -> string_of_int v
    | Bool(v) -> string_of_bool v
    | UOp(op, e) -> uOp_to_string op ^ expF_to_string e
    | BOp(op, e1, e2) -> expF_to_string e1 ^ bOp_to_string op ^ expF_to_string e2
    | Var x -> x
    | Let(x, e1, e2) -> "let" ^ x ^ " = " ^ expF_to_string e1 ^ " in " ^expF_to_string e2
    | Lam(x, t, e) -> x ^ " -> " ^ expF_to_string e 
    | App(e1, e2) -> "(" ^ expF_to_string e1 ^ ") " ^ expF_to_string e2
    | Ite(e1, e2, e3) -> "if " ^ expF_to_string e1 ^ " then " ^ expF_to_string e2 ^ " else " ^ expF_to_string e3
    | Process(c, eP, tS, xtl) -> c ^ " <- {\n" ^ expP_to_string eP ^ "}" ^ process_input_channels_to_string xtl ^ "\n"
    | LetRec(x, t, eF) -> "let rec " ^ x ^ " = " ^ expF_to_string eF

and process_input_channels_to_string xtl =
    if List.is_empty xtl then ""
    else " <- [" ^ label_tyS_list_to_string xtl ^ "]" 

and expP_to_string e =
    match e with 
    | SendF(c, eF, eP) -> "send " ^ c ^ " " ^ expF_to_string eF ^ ";\n" ^ expP_to_string eP
    | RecvF(x, tF, c, eP) -> x ^ " <- recv " ^ c ^ ";\n" ^ expP_to_string eP
    | SendS(c1, c2, eP1, eP2) -> "send " ^ c1 ^ " (" ^ c2 ^ " <- " ^ expP_to_string eP1 ^ ");\n" ^ expP_to_string eP2
    | RecvS(x, tS, c, eP) -> x ^ " <- recv " ^ c ^ ";\n" ^ expP_to_string eP
    | Close(c) -> "close " ^ c ^ "\n"
    | Wait(c, eP) -> "wait " ^ c ^ ";\n" ^ expP_to_string eP
    | Fwd(c1, c2, tS) -> "fwd " ^ c1 ^ " " ^ c2 ^ "\n"
    | Choice(c, labelprocesslist) -> "case " ^ c ^ " of\n" ^ label_process_list_to_string labelprocesslist 
    | ChoiceSelect(c, l, eP) -> c ^ "." ^ l ^ ";\n" ^ expP_to_string eP
    | Spawn(c, eF, cl, eP) -> c ^ " <- spawn " ^ expF_to_string eF ^ spawn_c_list_to_string cl  ^ ";\n" ^ expP_to_string eP

and label_process_list_to_string labelprocesslist = 
    match labelprocesslist with
    | [] -> ""
    | (l, eP)::labelprocesslist' -> l ^ ": (\n" ^ expP_to_string eP ^ ")\n" ^ label_process_list_to_string labelprocesslist' 

and spawn_c_list_to_string cl =
    if List.is_empty cl then ""
    else " [" ^ c_list_to_string cl ^ "]" 

and c_list_to_string cl =
    match cl with
    | [] -> ""
    | [c] -> c
    | c::cl' -> c ^ "; " ^ c_list_to_string cl'

let rec delta_to_string c = 
    match c with
    | [] -> "";
    | [(x, t)] -> x ^ ":" ^ tyS_to_string t
    | (x, t)::c' -> x ^ ":" ^ tyS_to_string t ^ "; " ^ delta_to_string c'

let rec psi_to_string c = 
    match c with
    | [] -> "";
    | [(x, t)] -> x ^ ":" ^ tyF_to_string t
    | (x, t)::c' -> x ^ ":" ^ tyF_to_string t ^ "; " ^ psi_to_string c'

let print_delta c = print_endline ("[" ^ delta_to_string c ^ "]")

let print_psi c = print_endline ("[" ^ psi_to_string c ^ "]")

let increment_depth f = 
    assert (f.currDepth <= f.maxDepth); 
    { f with currDepth = f.currDepth + 1 }

let debugF f ctxts goal tFocus curr_fun =
    if not f.printDebug then ()
    else let indent = String.make (f.currDepth * 2) ' ' in
        print_endline (indent ^ "currDepth: " ^ string_of_int f.currDepth); 
        match curr_fun with
        | "invertRightF" | "invertLeftF" | "focusDecideF" ->
            print_endline (indent ^ curr_fun ^ ": " ^ tyF_to_string goal);
            print_string (indent ^ "pa: "); print_psi ctxts.p.a;
            print_string (indent ^ "ps: "); print_psi ctxts.p.s
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
            print_string (indent ^ "da: "); print_delta ctxts.d.a;
            print_string (indent ^ "ds: "); print_delta ctxts.d.s
        | _ ->
            print_endline (indent ^ curr_fun ^ ": " ^ tyS_to_string goal);
            print_endline (indent ^ "tFocus: " ^ tyS_to_string tFocus)

let is_tyF_left_async t = 
    match t with
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
    { ctxts with p = { a = pa'; s = ps' } }

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

let rec subst e1 x e2 =
    match e1 with 
    | Var y -> if x=y then e2 else e1 
    | App(e, e') -> App (subst e x e2 , subst e' x e2)
    | Lam(y, t, e) -> if x <> y then Lam (y,t,(subst e x e2)) else e1 
    | _ -> raise (Fail("subst pattern matching not defined for " ^ expF_to_string e1))

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

let consume_channels ctxts cl =
    let rec consume_channels' ctxts' cl' sl =
        match cl' with
        | [] -> ctxts', sl
        | c'::cl'' -> 
            let ctxts'', s = consume_channel ctxts c' in
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

(* S[μt.S / t] *)
let rec unfold tUnfold stRecReplacement xReplace =
    match tUnfold with
    | STSendF(t1, t2) ->
        STSendF(t1, unfold t2 stRecReplacement xReplace)
    | STRecvF(t1, t2) ->
        STRecvF(t1, unfold t2 stRecReplacement xReplace)
    | STSendS(t1, t2) ->
        STSendS (unfold t1 stRecReplacement xReplace, unfold t2 stRecReplacement xReplace)
    | STRecvS(t1, t2) ->
        STRecvS (unfold t1 stRecReplacement xReplace, unfold t2 stRecReplacement xReplace)
    | STUnit ->
        STUnit
    | STExtChoice labelsesslist ->
        STExtChoice (List.map (fun (l, t) -> (l, unfold t stRecReplacement xReplace)) labelsesslist)
    | STIntChoice labelsesslist ->
        STIntChoice (List.map (fun (l, t) -> (l, unfold t stRecReplacement xReplace)) labelsesslist)
    | STRec(x, t) ->
        if x = xReplace then STRec(x, t)
        else STRec(x, unfold t stRecReplacement xReplace)
    | STRecVar(x) ->
        if x = xReplace then stRecReplacement
        else STRecVar(x)
    | _ -> raise (Fail "Unexpected STDeclr while unfolding recursive session-type")

(* inversion and focusing *)

(* flags + gamma-async; gamma-sync; psi-async; psi-async; delta-async; delta-sync |- P :: c : goal *)

let rec invertRightF f ctxts c goal =
    let f = increment_depth f in
    debugF f ctxts goal goal "invertRightF";
    match goal with
    | TArrow(t1, t2) ->
        let x = match t1 with
            | TRefinement(x, _, _) -> x
            | _ -> fresh_id() in 
        let ctxts1 = append_bindings_psi ctxts [(x, t1)] in
        begin match get_return_type t2 with
        | TProcess(_, STRec _) when not (List.mem_assoc f.xRecLam ctxts.p.s) ->
            let f, ctxts2 =
            try
                let (xRecFun, _) = List.find (fun (_, t) -> t = goal) ctxts.p.s in
                let f = { f with xRecLam = xRecFun } in
                f, ctxts1
            with Not_found ->
                let f = { f with xRecLam = fresh_function() } in
                let ctxts2 = append_bindings_psi ctxts1 [(f.xRecLam, goal)] in
                f, ctxts2
            in
            let* (f, ctxts', e) = invertRightF f ctxts2 c t2 in
            Choice.return (f, ctxts', LetRec(f.xRecLam, goal, Lam(x, t1, e)))
        | _ -> 
            let* (f, ctxts', e) = invertRightF f ctxts1 c t2 in
            Choice.return (f, ctxts', Lam(x, t1, e))
        end
    | TProcess(incsl, outs) ->
        begin try 
        let f = if f.xRecLam = "" then { f with xRecLam = find_binding_for_tyF ctxts goal } else f in
        let incsl = List.map (fun (c, s) -> if c = "" then (fresh_channel(), s) else (c, s)) incsl in
        let ctxts1 = append_bindings_delta ctxts incsl in
        let* (f, ctxts', e) = invertRightS f ctxts1 c outs in
        Choice.return (f, ctxts', Process(c, e, outs, incsl))
        with Not_found -> Choice.fail
        end
    | TDeclr(x) -> 
        begin try
            let t = List.assoc x ctxts.p.s in 
            invertRightF f ctxts c t
        with Not_found -> Choice.fail
        end
    | _ -> invertLeftF f ctxts c goal

and invertRightS f ctxts c goal = 
    let f = increment_depth f in
    debugS f ctxts goal goal "invertRightS";
    match goal with 
    | STRecvF(t1, t2) ->
        let x1 = fresh_id() in
        let ctxts1 = append_bindings_psi ctxts [(x1, t1)] in
        let* (f, ctxts', e) = invertRightS f ctxts1 c t2 in
        Choice.return (f, ctxts', RecvF(x1, t1, c, e))
    | STRecvS(t1, t2) -> 
        let c1 = fresh_channel() in
        let ctxts1 = append_bindings_delta ctxts [(c1, t1)] in
        let* (f, ctxts', e) = invertRightS f ctxts1 c t2 in
        let c1_consumed = List.assoc_opt c1 ctxts'.d.a = None && List.assoc_opt c1 ctxts'.d.s = None in
        let* () = Choice.guard c1_consumed in
        Choice.return (f, ctxts', RecvS(c1, t1, c, e))
    | STExtChoice(labelsesslist) ->
        let synth_branch (l, s) =
            let* (f', ctxts', eP) = invertRightS f ctxts c s in
            Choice.return (l, (f', ctxts', eP))
        in
        let* branches = ChoiceUtils.map_list synth_branch labelsesslist in
        let ctxtsl = List.map (fun (_, (_, ctxts', _)) -> ctxts') branches in
        let ctxts' = List.hd ctxtsl in
        let ctxts_equal = List.for_all (fun ctxtsn -> ctxtsn.d = ctxts'.d) (List.tl ctxtsl) in
        let* () = Choice.guard ctxts_equal in
        let labelproclist = List.map (fun (l, (_, _, e)) -> (l, e)) branches in
        Choice.return (f, ctxts', Choice(c, labelproclist))
    | STRec(x, t) ->
        if f.isUnfoldedRight then try
            Choice.mplus
                (
                (* wander + fwd *)
                let* (f, ctxts, eWander) = wander f ctxts c goal in
                let synthFwdCombination (c', t') = synthFwd f ctxts c' c t' in
                let* (f, ctxts', eFwd) = Choice.of_list (List.map synthFwdCombination ctxts.d.s) in
                let eWanderAndFwd = subst_continuation_exp eWander eFwd in
                Choice.return (f, ctxts', eWanderAndFwd)
                )
                (
                (* wander + spawn + forward *)
                let* (f, ctxts, eWander) = wander f ctxts c goal in
                let tRecLam = List.assoc f.xRecLam ctxts.p.s in
                let tProcess = get_return_type tRecLam in
                let spawnable_proc_list = List.filter(fun (_, t) -> 
                    let tReturn = get_return_type t in 
                    is_TProcess tReturn && get_TProcess_outs tReturn = get_TProcess_outs tProcess
                ) ctxts.p.s in
                let* (f, ctxts', eSpawn) = synthSpawn f ctxts c spawnable_proc_list in
                let f, ctxts'', eFwd = synthFwd f ctxts' (get_Spawn_c eSpawn) c t in
                let eSpawnAndFwd = subst_continuation_exp eSpawn eFwd in
                let eWanderAndSpawn = subst_continuation_exp eWander eSpawnAndFwd in
                Choice.return (f, ctxts'', eWanderAndSpawn)
                )
            with Not_found -> Choice.fail
        else 
            let tUnfolded = unfold t goal x in
            let f = {f with isUnfoldedRight = true } in
            invertRightS f ctxts c tUnfolded
    | STDeclr(x) ->
        begin try
            let t = List.assoc x ctxts.d.s in 
            invertRightS f ctxts c t
        with Not_found -> Choice.fail
        end
    | _ -> invertLeftS f ctxts c goal

and invertLeftF f ctxts c goal =
    let f = increment_depth f in
    debugF f ctxts goal goal "invertLeftF";
    match ctxts.p.a with
    | (x, t)::pa' -> Choice.fail
    | [] -> focusDecideF f ctxts c goal 

and invertLeftS f ctxts c goal =
    let f = increment_depth f in
    debugS f ctxts goal goal "invertLeftS";
    match ctxts.d.a with
    | (x, t)::da' ->
        let ctxts = { ctxts with d = { ctxts.d with a = da' } } in
        begin match t with 
        | STSendF(t1, t2) ->
            let x, c' = fresh_id(), fresh_channel() in
            let ctxts1 = append_bindings_psi ctxts [(x, t1)] in
            let ctxts2 = append_bindings_delta ctxts1 [(c, t2)] in
            let* (f, ctxts', e) = invertLeftS f ctxts2 c' goal in
            let c_consumed = List.assoc_opt c ctxts'.d.a = None && List.assoc_opt c ctxts'.d.s = None in
            let* () = Choice.guard c_consumed in
            Choice.return (f, ctxts', RecvF(x, t1, c, e))
        | STSendS(t1, t2) ->
            let x, c' = fresh_channel(), fresh_channel() in
            let ctxts1 = append_bindings_delta ctxts [(x, t1); (c, t2)] in
            let* (f, ctxts', e) = invertLeftS f ctxts1 c' goal in
            let c_consumed = List.assoc_opt c ctxts'.d.a = None && List.assoc_opt c ctxts'.d.s = None in
            let* () = Choice.guard c_consumed in
            Choice.return (f, ctxts', RecvS(x, t1, c, e))
        | STUnit -> 
            let c' = fresh_channel() in
            let* (f, ctxts', e) = invertLeftS f ctxts c' goal in
            Choice.return (f, ctxts', Wait(c, e))
        | STIntChoice(labelsesslist) ->
            let synth_branch (l, s) =
                let cn = fresh_channel() in
                let ctxtsn = append_bindings_delta ctxts [(cn, s)] in
                let* (f', ctxts', eP) = invertLeftS f ctxtsn c goal in
                let cn_consumed = List.assoc_opt cn ctxts'.d.a = None && List.assoc_opt cn ctxts'.d.s = None in
                let* () = Choice.guard cn_consumed in
                Choice.return (l, (f', ctxts', eP))
            in
            let* branches = ChoiceUtils.map_list synth_branch labelsesslist in
            let ctxtsl = List.map (fun (_, (_, ctxts', _)) -> ctxts') branches in
            let* () = Choice.guard (Option.is_some (deltas_are_equal ctxtsl)) in
            let labelproclist = List.map (fun (l, (_, _, e)) -> (l, e)) branches in
            Choice.return (f, List.hd ctxtsl, Choice(x, labelproclist))
        | _ -> Choice.fail
        end
    | [] -> focusDecideS f ctxts c goal

and focusDecideF f ctxts c goal = 
    debugF f ctxts goal goal "focusDecideF";
    let left_focuses = List.map (fun (xFoc, tFoc) -> focusLeftF f ctxts xFoc tFoc c goal) ctxts.p.s in
    let right_focus = focusRightF f ctxts c goal in
    ChoiceUtils.mplus_list (left_focuses @ [right_focus])

and focusDecideS f ctxts c goal = 
    debugS f ctxts goal goal "focusDecideS";
    let left_focuses = List.map (fun (xFoc, tFoc) -> focusLeftS f ctxts xFoc tFoc c goal) ctxts.d.s in
    let right_focus = focusRightS f ctxts c goal in
    ChoiceUtils.mplus_list (left_focuses @ [right_focus])

and focusRightF f ctxts c goal =
    let f = increment_depth f in
    debugF f ctxts goal goal "focusRightF";
    match goal with
    | TAtomic(TInt) -> Choice.return (f, ctxts, Int(1))
    | TAtomic(TBool) -> Choice.return (f, ctxts, Bool(true))
    | TRefinement(x, t1, t2) ->
        let solution = Cvc5adapter.solve ctxts.p.s goal in
        Choice.return (f, ctxts, solution)
    | _ -> invertRightF f ctxts c goal

and focusRightS f ctxts c goal =
    let f = increment_depth f in
    debugS f ctxts goal goal "focusRightS";
    match goal with 
    | STSendF(t1, t2) ->
        let* (f, ctxts', e1) = focusDecideF f ctxts c t1 in
        let* (f, ctxts'', e2) = focusRightS f ctxts' c t2 in
        Choice.return (f, ctxts'', SendF(c, e1 , e2))
    | STSendS(t1, t2) -> 
        let y = fresh_channel() in
        let* (f, ctxts', e1) = focusDecideS f ctxts y t1 in
        let* (f, ctxts'', e2) = focusRightS f ctxts' c t2 in
        Choice.return (f, ctxts'', SendS(c, y, e1 , e2))
    | STUnit ->
        Choice.return (f, ctxts, Close(c))
    | STIntChoice(labelsesslist) -> 
        let synth_choice_select (l, s) =
            let* (f', ctxts', e1) = focusRightS f ctxts c s in
            Choice.return (f', ctxts', ChoiceSelect(c, l, e1))
        in
        ChoiceUtils.map_mplus_list synth_choice_select labelsesslist
    | _ -> invertRightS f ctxts c goal (* goal is not right sync, therefore switch back to inversion phase *)

and focusLeftF f ctxts xFocus tFocus c goal =
    let f = increment_depth f in
    debugF f ctxts goal tFocus "focusLeftF";
    match tFocus with
    | TArrow(t1, t2) ->
        let y = fresh_id() in
        let* (f, ctxts', e2) = focusLeftF f ctxts y t2 c goal in
        let* (f, ctxts'', e1) = invertRightF f ctxts c t1 in
        Choice.return (f, ctxts'', subst e2 y (App(Var(xFocus), e1)))
    | TProcess _ | TAtomic _ | TRefinement _ | TDeclr _ -> 
        if tFocus = goal then Choice.return (f, ctxts, Var(xFocus))
        else Choice.fail

and focusLeftS f ctxts xFocus tFocus c goal =
    let f = increment_depth f in
    debugS f ctxts goal tFocus "focusLeftS";
    match tFocus with
    | STRecvF(t1, t2) ->
        let* (f, ctxts', e1) = invertRightF f ctxts c t1 in
        let* (f, ctxts'', e2) = focusLeftS f ctxts' xFocus t2 c goal in
        Choice.return (f, ctxts'', SendF(xFocus, e1, e2))
    | STRecvS(t1, t2) -> 
        let y = fresh_id() in
        let* (f, ctxts', e1) = invertRightS f ctxts y t1 in
        let* (f, ctxts'', e2) = focusLeftS f ctxts' xFocus t2 c goal in
        Choice.return (f, ctxts'', SendS(xFocus, y, e1, e2))
    | STExtChoice(labelsesslist) -> 
        let synth_choice_select (l, s) =
            let* (f', ctxts', e1) = focusLeftS f ctxts xFocus s c goal in
            Choice.return (f', ctxts', ChoiceSelect(xFocus, l, e1))
        in
        ChoiceUtils.map_mplus_list synth_choice_select labelsesslist
    | STRec(x, t) ->
        if f.isUnfoldedLeft then
            Choice.return (f, ctxts, Close(""))
        else
            let tUnfolded = unfold t goal x in
            let f = {f with isUnfoldedLeft = true } in
            focusLeftS f ctxts xFocus tUnfolded c goal
    | _ -> Choice.fail

(* wandering *)

and wander f ctxts c goal =
    debugS f ctxts goal goal "wander";
    let skipChoice = Choice.return (f, ctxts, Close("")) in
    let spawnChoice = wanderSpawn f ctxts c goal in
    let unfoldChoice = wanderUnfold f ctxts c goal in
    ChoiceUtils.mplus_list [skipChoice; spawnChoice; unfoldChoice]

and wanderSpawn f ctxts c goal =
    let spawnable_proc_list = List.filter (fun (_, t) -> is_TProcess (get_return_type t)) ctxts.p.s in
    synthSpawn f ctxts c spawnable_proc_list

and wanderUnfold f ctxts c goal =
    let recsessl = List.filter (fun (_, t) -> is_STRec t) ctxts.d.s in
    let synth_unfolds (c', t') = focusLeftS f ctxts c' t' c goal in
    ChoiceUtils.map_mplus_list synth_unfolds recsessl

(* reusable expression synthesis *)

(** 
synthesizes possible spawn expressions which output a desired session-type, containing a placeholder continuation expression
@param spawnable_proc_list: list of TProcess(insl, outs)'s with equivalent outs session-types, which will be provided by the spawned channel
*)
and synthSpawn f ctxts c spawnable_proc_list =
    let cSpawn = fresh_channel() in
    let* (x, t) = Choice.of_list spawnable_proc_list in
    let tProcess = get_return_type t in
    let insl = get_TProcess_insl tProcess in
    let outs = get_TProcess_outs tProcess in
    let* (_, ctxts', eApp) = focusLeftF f ctxts x t c tProcess in
    let* (ctxts'', incl) = consume_channels_by_tyS ctxts' insl in
    let ctxts''' = append_bindings_delta ctxts'' [(cSpawn, outs)] in
    Choice.return (f, ctxts''', Spawn(cSpawn, eApp, incl, Close("")))

and synthFwd f ctxts cToFwd c t =
    let ctxts', _ = consume_channel ctxts cToFwd in
    f, ctxts', Fwd(cToFwd, c, t)

(* entry point *)

let synth n_sol p d goal = 
    let f, ctxts = initialize_flags 100 true, initialize_ctxts in
    let ctxts = append_bindings_psi ctxts p in
    let ctxts = append_bindings_delta ctxts d in
    let expl = List.map (fun (_, _, e) -> e)
        (match invertRightF f ctxts (fresh_channel()) goal |> Choice.run_n n_sol with
        | [] -> raise (Fail "No valid expression was able to be synthesized for the provided type")
        | solutions -> solutions) |> List.rev
    in
    let i = ref 0 in
    print_newline ();
    List.iter (fun e -> print_endline (string_of_int !i ^ ":"); print_endline (expF_to_string e ^ "\n"); incr i) expl;
    if List.length expl = 1 then List.hd expl
    else (
        let rec choose_exp() =
            print_string "\nSelect solution: "; 
            let chosen_exp = read_int() in
            if chosen_exp < 0 || chosen_exp >= List.length expl then choose_exp()
            else List.nth expl chosen_exp
        in choose_exp()
    )

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