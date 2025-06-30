module Language = Language

open Language

exception Fail of string

(* Records *)

type flags = { isUnfolded : bool; xRecLam : id; currDepth : int; maxDepth : int }
    
type gamma = { a : (id * tyS) list; s :  (id * tyS) list }
    
type psi = { a : (id * tyF) list; s :  (id * tyF) list }
    
type delta = { a : (id * tyS) list; s :  (id * tyS) list }
    
type contexts = { g : gamma; p : psi; d : delta }

(* Auxiliary functions *)

let initialize_flags_and_ctxts maxDepth =
    let f : flags = { isUnfolded = false ; xRecLam = ""; currDepth = 0; maxDepth = maxDepth } in
    let g : gamma = { a = []; s = [] } in
    let p : psi = { a = []; s = [] } in
    let d : delta = { a = []; s = [] } in
    let ctxts : contexts = { g = g; p = p; d = d } in
    f, ctxts

let fresh_id = 
    let unique = ref (-1) in
    fun () -> (incr unique; "x" ^ (string_of_int !unique))

let fresh_function = 
    let unique = ref (-1) in
    fun () -> (incr unique; "f" ^ (string_of_int !unique))

let fresh_channel = 
    let unique = ref (-1) in
    fun () -> (incr unique; "c" ^ (string_of_int !unique))

let rec tyA_to_string t =
    match t with
    | TInt -> "int"
    | TBool -> "bool"

and tyR_to_string t =
    match t with
    | RTAnd(t1, t2) -> tyR_to_string t1 ^ " && " ^ tyR_to_string t2
    | RTOr(t1, t2) -> tyR_to_string t1 ^ " || " ^ tyR_to_string t2
    | RTEq(t1, t2) -> tyR_to_string t1 ^ " = " ^ tyR_to_string t2
    | RTGr(t1, t2) -> tyR_to_string t1 ^ " > " ^ tyR_to_string t2
    | RTLt(t1, t2) -> tyR_to_string t1 ^ " < " ^ tyR_to_string t2
    | RTGrE(t1, t2) -> tyR_to_string t1 ^ " >= " ^ tyR_to_string t2
    | RTLtE (t1, t2) -> tyR_to_string t1 ^ " <= " ^ tyR_to_string t2
    | RTSum (t1, t2) -> tyR_to_string t1 ^ " + " ^ tyR_to_string t2
    | RTSub (t1, t2) -> tyR_to_string t1 ^ " - " ^ tyR_to_string t2
    | RTMult(t1, t2) -> tyR_to_string t1 ^ " * " ^ tyR_to_string t2
    | RTDiv (t1, t2) -> tyR_to_string t1 ^ " / " ^ tyR_to_string t2
    | RTInt(v) -> string_of_int v
    | RTBool(v) -> string_of_bool v
    | RTVar(x) -> x

and tyF_to_string t =
    match t with 
    | TAtomic(t) -> tyA_to_string t
    | TRefinement(x, t1, t2) -> "{" ^ x ^ ":" ^ tyA_to_string t1 ^ " | " ^ tyR_to_string t2 ^ "}"
    | TArrow(t1, t2) -> tyF_to_string t1 ^ " -> " ^ tyF_to_string t2
    | TProcess(tl, t) -> "{" ^ tyS_list_to_string tl ^ " |- " ^ tyS_to_string t ^ "}"

and tyS_to_string t =
    match t with 
    | STDeclr(x, t1, t2) -> "stype " ^ x ^ " = " ^ tyS_to_string t1 ^ ";\n" ^ tyS_to_string t2
    | STSendF(t1, t2) -> tyF_to_string t1 ^ " ∧ " ^ tyS_to_string t2
    | STRecvF(t1, t2) -> tyF_to_string t1 ^ " ⊃ " ^ tyS_to_string t2
    | STSendS(t1, t2) -> tyS_to_string t1 ^ " ⊗ " ^ tyS_to_string t2
    | STRecvS(t1, t2) -> tyS_to_string t1 ^ " -o " ^ tyS_to_string t2
    | STExtChoice(xtl) -> "&{" ^ label_tyS_list_to_string xtl ^ "}"
    | STIntChoice(xtl) -> "⊕{" ^ label_tyS_list_to_string xtl ^ "}"
    | STRec(x, t) -> "𝜇" ^ x ^ "." ^ tyS_to_string t
    | STRecVar(x) -> x
    | STUnit -> "1"

and tyS_list_to_string tl =
    match tl with
    | [] -> ""
    | [t] -> tyS_to_string t
    | t::tl' -> tyS_to_string t ^ ", " ^ tyS_list_to_string tl'

and label_tyS_list_to_string xtl =
    match xtl with
    | [] -> ""
    | [(l, t)] -> l ^ ":" ^ tyS_to_string t
    | (l, t)::xtl' -> l ^ ":" ^ tyS_to_string t ^ ", " ^ label_tyS_list_to_string xtl' 

let rec expF_to_string e =
    match e with 
    | Int(v) -> string_of_int v
    | Bool(v) -> string_of_bool v
    | Not(e1) -> "!" ^ expF_to_string e1
    | And(e1, e2) -> expF_to_string e1 ^ " && " ^ expF_to_string e2
    | Or(e1, e2) -> expF_to_string e1 ^ " || " ^ expF_to_string e2
    | Eq(e1, e2) -> expF_to_string e1 ^ " == " ^ expF_to_string e2
    | Gr(e1, e2) -> expF_to_string e1 ^ " > " ^ expF_to_string e2
    | Lt(e1, e2) -> expF_to_string e1 ^ " < " ^ expF_to_string e2
    | GrE(e1, e2) -> expF_to_string e1 ^ " >= " ^ expF_to_string e2
    | LtE(e1, e2) -> expF_to_string e1 ^ " <= " ^ expF_to_string e2
    | Sum(e1, e2) -> expF_to_string e1 ^ " + " ^ expF_to_string e2
    | Sub(e1, e2) -> expF_to_string e1 ^ " - " ^ expF_to_string e2
    | Mult(e1, e2) -> expF_to_string e1 ^ " * " ^ expF_to_string e2
    | Div(e1, e2) -> expF_to_string e1 ^ " / " ^ expF_to_string e2
    | Var x -> x
    | Let(x, e1, e2) -> "let" ^ x ^ " = " ^ expF_to_string e1 ^ " in " ^expF_to_string e2
    | Lam(x, t, e) -> x ^ " -> " ^ expF_to_string e 
    | App(e1, e2) -> "(" ^ expF_to_string e1 ^ ") " ^ expF_to_string e2
    | Ite(e1, e2, e3) -> "if " ^ expF_to_string e1 ^ " then " ^ expF_to_string e2 ^ " else " ^ expF_to_string e3
    | Process(c, eP, tS, xtl) -> c ^ " <- {\n" ^ expP_to_string eP ^ "}" ^ process_input_channels_to_string xtl ^ "\n"
    | LetRec(x, eF) -> "let rec " ^ x ^ " = " ^ expF_to_string eF

and process_input_channels_to_string xtl =
    if List.is_empty xtl then ""
    else " <- [" ^ label_tyS_list_to_string xtl ^ "]" 

and expP_to_string e =
    match e with 
    | SendF(c, eF, eP) -> "send " ^ c ^ " " ^ expF_to_string eF ^ ";\n" ^ expP_to_string eP
    | RecvF(x, c, eP) -> x ^ " <- recv " ^ c ^ ";\n" ^ expP_to_string eP
    | SendS(c1, c2, eP1, eP2) -> "send " ^ c1 ^ " (" ^ c2 ^ " <- " ^ expP_to_string eP1 ^ ");\n" ^ expP_to_string eP2
    | RecvS(x, c, eP) -> x ^ " <- recv " ^ c ^ ";\n" ^ expP_to_string eP
    | Close(c) -> "close " ^ c ^ ";\n"
    | Wait(c, eP) -> "wait " ^ c ^ ";\n" ^ expP_to_string eP
    | Fwd(c1, c2, tS) -> "fwd " ^ c1 ^ " " ^ c2 ^ "\n"
    | Choice(c, labelprocesslist) -> "case " ^ c ^ " of [" ^ label_process_list_to_string labelprocesslist ^ "]" 
    | ChoiceSelect(c, l, eP) -> c ^ "." ^ l ^ ";\n" ^ expP_to_string eP
    | Spawn(c, eF, cl, eP) -> c ^ " <- spawn " ^ expF_to_string eF ^ ";\n" ^ expP_to_string eP

and label_process_list_to_string labelprocesslist = 
    match labelprocesslist with
    | [] -> ""
    | [(l, eP)] -> l ^ ":" ^ expP_to_string eP
    | (l, eP)::labelprocesslist' -> l ^ ":" ^ expP_to_string eP ^ "; " ^ label_process_list_to_string labelprocesslist' 

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

let print_delta c = print_endline (delta_to_string c)

let print_psi c = print_endline (psi_to_string c)

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

let rec deltas_are_equal inversions prevCtxts = 
    match inversions with
    | (_, ctxts', _)::inversions' -> List.equal (=) prevCtxts.d.a ctxts'.d.a && List.equal (=) prevCtxts.d.s ctxts'.d.s && deltas_are_equal inversions' ctxts'
    | [] -> true

let rec build_label_expP_list labelsesslist inversions =
    match labelsesslist, inversions with
    | (l, _)::labelsesslist', (_, _, expP)::inversions' -> (l, expP)::build_label_expP_list labelsesslist' inversions'
    | [], [] | [], _ | _, [] -> []

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

let consume_channel d c =
    let da, ds = d in
    try begin
        let da', s = get_and_remove c da in
        (da', ds), s
    end 
    with Not_found -> try begin
        let ds', s = get_and_remove c ds in
        (da, ds'), s
    end
    with Not_found -> raise (Fail("consume_channel: channel " ^ c ^ " not found"))

let rec consume_channels_with_sessions d insl consumed_csl =
    let da, ds = d in
    match insl with
    | s::insl' -> 
        begin
            try begin
                let c = get_first_channel_with_session da s in
                let da' = List.remove_assoc c da in
                consume_channels_with_sessions (da', ds) insl' ((c, s)::consumed_csl)
            end 
            with Fail _ -> try begin
                let c = get_first_channel_with_session ds s in
                let ds' = List.remove_assoc c ds in
                consume_channels_with_sessions (da, ds') insl' ((c, s)::consumed_csl)
            end
            with Fail m -> raise (Fail m)
        end
    | [] -> d, consumed_csl

and get_first_channel_with_session d sFind =
    match d with
    | (c, s)::d' -> if sFind = s then c else get_first_channel_with_session d' sFind
    | [] -> raise (Fail("No existing binding for a channel with session type " ^ tyS_to_string sFind))

let rec get_TArrow_return_type t =
    match t with
    | TArrow(t1, t2) -> get_TArrow_return_type t2
    | _ -> t

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

(* Inversion and Focusing *)

(* flags + gamma-async; gamma-sync; psi-async; psi-async; delta-async; delta-sync |- P :: c : goal *)
let rec invertRightS f ctxts c goal = 
    print_endline ("invertRightS: " ^ tyS_to_string goal);
    print_string "  da: "; print_delta ctxts.d.a;
    print_string "  ds: "; print_delta ctxts.d.s;
    let f = { f with currDepth = f.currDepth + 1 } in
    print_endline ("  currDepth: " ^ string_of_int f.currDepth);
    assert (f.currDepth <= f.maxDepth);
    match goal with 
    | STRecvF(t1, t2) ->
        let x = fresh_id() in
        let ctxts1 = append_bindings_psi ctxts [(x, t1)] in
        let f, ctxts', e = invertRightS f ctxts1 c t2 in
        assert (List.assoc_opt x ctxts'.d.a = None && List.assoc_opt x ctxts'.d.s = None);
        f, ctxts', RecvF(x, c, e)
    | STRecvS(t1, t2) -> 
        let x = fresh_id() in
        let ctxts1 = append_bindings_delta ctxts [(x, t1)] in
        let f, ctxts', e = invertRightS f ctxts1 c t2 in
        assert (List.assoc_opt x ctxts'.d.a = None && List.assoc_opt x ctxts'.d.s = None);
        f, ctxts', RecvS(x, c, e)
    | STExtChoice(labelsesslist) ->
        let inversions = List.map (fun (l, s) -> invertRightS f ctxts c s) labelsesslist in
        let f, ctxts1, _ = List.hd inversions in
        assert (deltas_are_equal (List.tl inversions) ctxts);
        let labelprocesslist = build_label_expP_list labelsesslist inversions in
        f, ctxts1, Choice(c, labelprocesslist)
    | STRec(x, t) ->
        if f.isUnfolded then
            let cRec = fresh_channel() in
            let tRecLam = List.assoc f.xRecLam ctxts.p.s in
            let tReturn = get_TArrow_return_type tRecLam in
            let f, ctxts, eApp = focusLeftF f ctxts f.xRecLam tRecLam c tReturn in
            f, ctxts, Spawn(cRec, eApp, [], Fwd(cRec, c, t))
        else 
            let tUnfolded = unfold t goal x in
            let f = {f with isUnfolded = true } in
            invertRightS f ctxts c tUnfolded
    | STDeclr(x, t1, t2) ->
        let ctxts1 = append_bindings_gamma ctxts [(x, t1)] in
        invertRightS f ctxts1 c t2
    | _ -> invertLeftS f ctxts c goal

and invertRightF f ctxts c goal =
    print_endline ("invertRightF: " ^ tyF_to_string goal);
    print_string "  pa: "; print_psi ctxts.p.a;
    print_string "  ps: "; print_psi ctxts.p.s;
    let f = { f with currDepth = f.currDepth + 1 } in
    print_endline ("  currDepth: " ^ string_of_int f.currDepth);
    assert (f.currDepth <= f.maxDepth);
    match goal with
    | TArrow(t1, t2) ->
        let x = match t1 with
            | TRefinement(x, _, _) -> x
            | _ -> fresh_id() in 
        let ctxts1 = append_bindings_psi ctxts [(x, t1)] in
        begin match get_TArrow_return_type t2 with
        | TProcess(_, STRec _) when not (List.mem_assoc f.xRecLam ctxts.p.a || List.mem_assoc f.xRecLam ctxts.p.s) ->
                let f = { f with xRecLam = fresh_function() } in
                let ctxts2 = append_bindings_psi ctxts1 [(f.xRecLam, goal)] in
                let f, ctxts', e = invertRightF f ctxts2 c t2 in
                f, ctxts', LetRec(f.xRecLam, Lam(x, t1, e))
        | _ -> 
            let f, ctxts', e = invertRightF f ctxts1 c t2 in
            f, ctxts', Lam(x, t1, e)
        end
    | TProcess(insl, outs) ->
        let incsl = List.map (fun (s) -> (fresh_channel(), s)) insl in
        let ctxts1 = append_bindings_delta ctxts incsl in
        let f, ctxts', e = invertRightS f ctxts1 c outs in
        f, ctxts', Process(c, e, outs, incsl)
    | _ -> invertLeftF f ctxts c goal

and invertLeftS f ctxts c goal =
    print_endline ("invertLeftS: " ^ tyS_to_string goal);
    print_string "  da: "; print_delta ctxts.d.a;
    print_string "  ds: "; print_delta ctxts.d.s;
    let f = { f with currDepth = f.currDepth + 1 } in
    print_endline ("  currDepth: " ^ string_of_int f.currDepth);
    assert (f.currDepth <= f.maxDepth);
    match ctxts.d.a with
    | (x, t)::da' ->
        let ctxts = { ctxts with d = { a = da'; s = ctxts.d.s } } in
        begin match t with 
        | STSendF(t1, t2) ->
            let x, c' = fresh_id(), fresh_channel() in
            let ctxts1 = append_bindings_psi ctxts [(x, t1)] in
            let ctxts2 = append_bindings_delta ctxts1 [(c, t2)] in
            let f, ctxts', e = invertLeftS f ctxts2 c' goal in
            assert (List.assoc_opt x ctxts'.d.a = None && List.assoc_opt x ctxts'.d.s = None && List.assoc_opt c ctxts'.d.a = None && List.assoc_opt c ctxts'.d.s = None);
            f, ctxts', RecvF(x, c, e)
        | STSendS(t1, t2) ->
            let x, c' = fresh_channel(), fresh_channel() in
            let ctxts1 = append_bindings_delta ctxts [(x, t1); (c, t2)] in
            let f, ctxts', e = invertLeftS f ctxts1 c' goal in
            assert (List.assoc_opt x ctxts'.d.a = None && List.assoc_opt x ctxts'.d.s = None && List.assoc_opt c ctxts'.d.a = None && List.assoc_opt c ctxts'.d.s = None);
            f, ctxts', RecvS(x, c, e)
        | STUnit -> 
            let c' = fresh_channel() in
            let f, ctxts', e = invertLeftS f ctxts c' goal in
            f, ctxts', Wait(c, e)
        | STIntChoice(labelsesslist) ->
            let inversions = List.map (fun (l, s) -> 
                let xn = fresh_id() in
                let ctxts1 = append_bindings_delta ctxts [(xn, s)] in
                let f, ctxts', e = invertLeftS f ctxts1 c goal in
                assert (List.assoc_opt xn ctxts'.d.a = None && List.assoc_opt xn ctxts'.d.s = None);
                f, ctxts', e
            ) labelsesslist in
            let f, ctxts1, _ = List.hd inversions in
            assert (deltas_are_equal (List.tl inversions) ctxts1);
            let labelprocesslist = build_label_expP_list labelsesslist inversions in
            f, ctxts1, Choice(x, labelprocesslist)
        | _ -> raise (Fail("invertLeft: somehow a sync type wound up in async context"))
        end
    | [] -> focusDecideS f ctxts ctxts.d.s c goal

and invertLeftF f ctxts c goal =
    print_endline ("invertLeftF: " ^ tyF_to_string goal);
    print_string "  pa: "; print_psi ctxts.p.a;
    print_string "  ps: "; print_psi ctxts.p.s;
    let f = { f with currDepth = f.currDepth + 1 } in
    print_endline ("  currDepth: " ^ string_of_int f.currDepth);
    assert (f.currDepth <= f.maxDepth);
    match ctxts.p.a with
    | (x, t)::pa' ->
        begin match t with
        | _ -> raise (Fail("invertLeftF: pattern matching not defined for " ^ tyF_to_string t))
        end
    | [] -> focusDecideF f ctxts ctxts.p.s c goal 

and focusDecideS f ctxts ds' c goal = 
    print_endline ("focusDecideS: " ^ tyS_to_string goal);
    print_string "  ds: "; print_delta ds';
    match ds' with
    | [] -> focusRightS f ctxts c goal
    | (xFoc, tFoc)::ds'' -> try 
            if is_tyS_left_async goal then focusRightS f ctxts c goal
            else focusLeftS f ctxts xFoc tFoc c goal
        with Fail _ -> focusDecideS f ctxts ds'' c goal

and focusDecideF f ctxts ps' c goal = 
    print_endline ("focusDecideF: " ^ tyF_to_string goal);
    print_string "  ps: "; print_psi ctxts.p.s;
    print_string "  ps': "; print_psi ps';
    match ps' with
    | [] -> focusRightF f ctxts c goal
    | (xFoc, tFoc)::ps'' -> try 
            if is_tyF_left_async goal then focusRightF f ctxts c goal
            else focusLeftF f ctxts xFoc tFoc c goal
        with Fail _ -> focusDecideF f ctxts ps'' c goal

and focusRightS f ctxts c goal =
    print_endline ("focusRightS: " ^ tyS_to_string goal);
    let f = { f with currDepth = f.currDepth + 1 } in
    print_endline ("  currDepth: " ^ string_of_int f.currDepth);
    assert (f.currDepth <= f.maxDepth);
    match goal with 
    | STSendF(t1, t2) ->
        let f, ctxts', e1 = focusDecideF f ctxts ctxts.p.s c t1 in
        let f, ctxts'', e2 = focusRightS f ctxts' c t2 in
        f, ctxts'', SendF(c, e1 , e2)
    | STSendS(t1, t2) -> 
        let y = fresh_channel() in
        let f, ctxts', e1 = focusRightS f ctxts y t1 in
        let f, ctxts'', e2 = focusRightS f ctxts' c t2 in
        f, ctxts'', SendS(c, y, e1 , e2)
    | STUnit ->
        f, ctxts, Close(c)
    | STIntChoice(labelsesslist) -> 
        let rec iter_labels labelsesslist =
            match labelsesslist with
            | (l, s)::labelsesslist' ->
                begin try
                    let f, ctxts', e1 = focusRightS f ctxts c s in
                    f, ctxts', ChoiceSelect(c, l, e1)
                    with Fail _ -> try iter_labels labelsesslist'
                    with Fail m -> raise (Fail m)
                end
            | [] -> raise (Fail "focusLeft: TIntChoice has no valid label-sess option")
        in iter_labels labelsesslist
    | _ -> invertRightS f ctxts c goal (* goal is not right sync, therefore switch back to inversion phase *)

and focusRightF f ctxts c goal =
    print_endline ("focusRightF: " ^ tyF_to_string goal);
    let f = { f with currDepth = f.currDepth + 1 } in
    print_endline ("  currDepth: " ^ string_of_int f.currDepth);
    assert (f.currDepth <= f.maxDepth);
    match goal with
    | TAtomic(TInt) -> f, ctxts, Int(1)
    | TAtomic(TBool) -> f, ctxts, Bool(true)
    | TRefinement(x, t1, t2) ->
        let solution = Cvc5adapter.solve ctxts.p.s goal in
        f, ctxts, solution
    | _ -> invertRightF f ctxts c goal

and focusLeftS f ctxts xFocus tFocus c goal =
    print_endline ("focusLeftS: " ^ tyS_to_string tFocus);
    let f = { f with currDepth = f.currDepth + 1 } in
    print_endline ("  currDepth: " ^ string_of_int f.currDepth);
    assert (f.currDepth <= f.maxDepth);
    match tFocus with
    | STRecvF(t1, t2) ->
        let f, ctxts', e2 = focusLeftS f ctxts xFocus t2 c goal in
        let f, ctxts'', e1 = invertRightF f ctxts' c t1 in
        f, ctxts'', SendF(xFocus, e1, e2) 
    | STRecvS(t1, t2) -> 
        let y = fresh_id() in
        let f, ctxts', e2 = focusLeftS f ctxts xFocus t2 c goal in
        let f, ctxts'', e1 = invertRightS f ctxts' y t1 in
        f, ctxts'', SendS(xFocus, y, e1, e2) 
    | STExtChoice(labelsesslist) -> 
        let rec iter_labels labelsesslist =
            match labelsesslist with
            | (l, s)::labelsesslist' ->
                begin try
                    let f, ctxts', e = focusLeftS f ctxts xFocus s c goal in
                    f, ctxts', ChoiceSelect(xFocus, l, e)
                    with Fail _ -> try iter_labels labelsesslist'
                    with Fail m -> raise (Fail m)
                end
            | [] -> raise (Fail "focusLeft: TExtChoice has no valid label-sess option")
        in iter_labels labelsesslist
    | _ -> raise (Fail("focusLeftS: somehow foc type is left async: " ^ tyS_to_string tFocus))

and focusLeftF f ctxts xFocus tFocus c goal =
    print_endline ("focusLeftF: tFocus: " ^ tyF_to_string tFocus ^ "    goal: " ^ tyF_to_string goal);
    let f = { f with currDepth = f.currDepth + 1 } in
    print_endline ("  currDepth: " ^ string_of_int f.currDepth);
    assert (f.currDepth <= f.maxDepth);
    match tFocus with
    | TArrow(t1, t2) ->
        let y = fresh_id() in
        let f, ctxts', e2 = focusLeftF f ctxts y t2 c goal in
        let f, ctxts'', e1 = invertRightF f ctxts c t1 in
        f, ctxts'', subst e2 y (App(Var(xFocus), e1))
    | TProcess _ | TAtomic _ | TRefinement _ -> 
        if tFocus = goal then f, ctxts, Var(xFocus)
        else raise (Fail "tFocus != goal")

let synth goal = 
    let f, ctxts = initialize_flags_and_ctxts 100 in
    let f, ctxts', e = invertRightF f ctxts (fresh_channel()) goal in
    if List.is_empty ctxts'.d.a && List.is_empty ctxts'.d.s then e
    else raise (Fail("Synthesized expression did not use all linear resources:\n  da: " ^ delta_to_string ctxts'.d.a ^ "\n  ds: " ^ delta_to_string ctxts'.d.s ^ "\n"))

let synth_ctxt p d goal = 
    let f, ctxts = initialize_flags_and_ctxts 100 in
    let f, ctxts', e = invertRightF f ctxts (fresh_channel()) goal in
    if List.is_empty ctxts'.d.a && List.is_empty ctxts'.d.s then e
    else raise (Fail("Synthesized expression did not use all linear resources:\n  da: " ^ delta_to_string ctxts'.d.a ^ "\n  ds: " ^ delta_to_string ctxts'.d.s ^ "\n"))

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