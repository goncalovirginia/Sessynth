exception Fail of string

(* Types and terms *)

type id = string

type tyF = (* ordinary functional types (F) *)
    | TInt (* integer *)
    | TBool (* boolean *)
    | TArrow of tyF * tyF (* F1 -> F2 *)
    | TProcess of tyS list * tyS (* { S1, ..., Sn |- P :: c : S } *)

and tyS = (* channel/session types (S) *)
    | STSendF of tyF * tyS (* F ∧ S *)
    | STRecvF of tyF * tyS (* F ⊃ S *)
    | STSendS of tyS * tyS (* S1 ⊗ S2 *)
    | STRecvS of tyS * tyS (* S1 -o S2 *)
    | STUnit (* 1 *)
    | STExtChoice of (id * tyS) list (* &{ l1:S1, ..., ln:Sn } *)
    | STIntChoice of (id * tyS) list (* ⊕{ l1:S1, ..., ln:Sn } *)
    | STRec of id * tyS (* mu t . S *)
    | STRecVar of id (* t *)
    | STDeclr of id * tyS * tyS (* stype x = S1; S2 *)

type expF = (* functional terms (M) *)
    | Int of int
    | Bool of bool
    | Var of id (* x *)
    | Let of id * expF * expF (* let x = M1 in M2 *)
    | Lam of id * tyF * expF (* fun x:F -> M *)
    | App of expF  * expF (* (M1) M2 *)
    | Process of id * expP * tyS * (id * tyS) list (* c <- {P :: c : S} <- [c1:S1; ...; cn:Sn] (opaque functional value, P not evaluated) *)
    | LetRec of id * expF (* let rec x = M1 *)

and expP = (* process terms (P) *)
    | SendF of id * expF * expP (* send c M; P : F ∧ S *)
    | RecvF of id * id * expP (* x:F <- recv c; P : F ⊃ S *)
    | SendS of id * id * expP * expP (* send c1 c2 P2; P : S1 ⊗ S2 *)
    | RecvS of id * id * expP (* x:S <- recv c; P : S1 -o S2 *)
    | Close of id (* close c : 1 *)
    | Wait of id * expP (* wait c; P *)
    | Fwd of id * id * tyS (* fwd c1 c2 :: c2 : S1 *)
    | Choice of id * (id * expP) list (* case c of li:Pi :: c : &{ l1:S1, ..., ln:Sn } *)
    | ChoiceSelect of id * id * expP (* c.l; P :: c : ⊕{ l1:S1, ..., ln:Sn } *)
    | Spawn of id * expF * id list * expP (* c <- spawn M [c1; ...; cn]; P *)
    
(* Records *)

type flags = { isUnfolded : bool; xRecLam : id }

type gamma = { a : (id * tyS) list; s :  (id * tyS) list }

type psi = { a : (id * tyF) list; s :  (id * tyF) list }

type delta = { a : (id * tyS) list; s :  (id * tyS) list }

type contexts = { g : gamma; p : psi; d : delta }

(* Auxiliary functions *)

let fresh_id = 
    let unique = ref (-1) in
    fun () -> (incr unique; "x" ^ (string_of_int !unique))

let fresh_channel = 
    let unique = ref (-1) in
    fun () -> (incr unique; "c" ^ (string_of_int !unique))

let rec tyF_to_string t =
    match t with 
    | TInt -> "TInt"
    | TBool -> "TBool"
    | TArrow(t1, t2) -> tyF_to_string t1 ^ " -> " ^ tyF_to_string t2
    | TProcess(tl, t) -> "{ " ^ tyS_list_to_string tl ^ " |- " ^ tyS_to_string t ^ " }"

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
    | Var x -> x
    | Let(x, e1, e2) -> "let" ^ x ^ " = " ^ expF_to_string e1 ^ " in " ^expF_to_string e2
    | Lam(x, t, e) -> x ^ ":" ^ tyF_to_string t ^ " -> " ^ expF_to_string e 
    | App(e1, e2) -> "(" ^ expF_to_string e1 ^ ") " ^ expF_to_string e2
    | Process(c, eP, tS, xtl) -> c ^ " <- {\n" ^ expP_to_string eP ^ "} <- [" ^ label_tyS_list_to_string xtl ^ "]\n"
    | LetRec(x, eF) -> "let rec " ^ x ^ " = " ^ expF_to_string eF

and expP_to_string e =
    match e with 
    | SendF(c, eF, eP) -> "send " ^ c ^ " " ^ expF_to_string eF ^ ";\n" ^ expP_to_string eP
    | RecvF(x, c, eP) -> x ^ " <- recv " ^ c ^ ";\n" ^ expP_to_string eP
    | SendS(c1, c2, eP1, eP2) -> "send " ^ c1 ^ " (" ^ c2 ^ " <- " ^ expP_to_string eP1 ^ ");\n" ^ expP_to_string eP2
    | RecvS(x, c, eP) -> x ^ " <- recv " ^ c ^ ";\n" ^ expP_to_string eP
    | Close(c) -> "close " ^ c ^ ";\n"
    | Wait(c, eP) -> "wait " ^ c ^ ";\n" ^ expP_to_string eP
    | Fwd(c1, c2, tS) -> "fwd " ^ c1 ^ " " ^ c2 ^ ":" ^ tyS_to_string tS ^ "\n"
    | Choice(c, labelprocesslist) -> "case " ^ c ^ " of [" ^ label_process_list_to_string labelprocesslist ^ "]" 
    | ChoiceSelect(c, l, eP) -> c ^ "." ^ l ^ ";\n" ^ expP_to_string eP
    | Spawn(c, eF, cl, eP) -> c ^ " <- spawn " ^ expF_to_string eF ^ " [" ^ c_list_to_string cl ^ "];\n" ^ expP_to_string eP

and label_process_list_to_string labelprocesslist = 
    match labelprocesslist with
    | [] -> ""
    | [(l, eP)] -> l ^ ":" ^ expP_to_string eP
    | (l, eP)::labelprocesslist' -> l ^ ":" ^ expP_to_string eP ^ ", " ^ label_process_list_to_string labelprocesslist' 

and c_list_to_string cl =
    match cl with
    | [] -> ""
    | [c] -> c
    | c::cl' -> c ^ ", " ^ c_list_to_string cl'

let rec delta_to_string c = 
    match c with
    | [] -> "";
    | (x, t)::c' -> x ^ ":" ^ (tyS_to_string t) ^ "; " ^ delta_to_string c'

let rec psi_to_string c = 
    match c with
    | [] -> "";
    | (x, t)::c' -> x ^ ":" ^ (tyF_to_string t) ^ "; " ^ psi_to_string c'

let rec print_delta c = print_endline (delta_to_string c)

let rec print_psi c = print_endline (psi_to_string c)

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

let rec append_bindings_gamma ctxts bindings =
    let ga', gs' = append_bindings (ctxts.g.a, ctxts.g.s) bindings is_tyS_left_async in
    { ctxts with g = { a = ga'; s = gs' } }

let rec append_bindings_psi ctxts bindings = 
    let pa', ps' = append_bindings (ctxts.p.a, ctxts.p.s) bindings is_tyF_left_async in
    { ctxts with p = { a = pa'; s = ps' } }

let rec append_bindings_delta ctxts bindings =
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

(* Focused type-driven synthesizer *)

module Sessynth = struct 

(* flags + gamma-async; gamma-sync; psi-async; psi-async; delta-async; delta-sync |- P :: c : goal *)
let rec invertRightS f ctxts c goal = 
    print_endline ("invertRightS: " ^ tyS_to_string goal);
    print_string "  da: "; print_delta ctxts.d.a;
    print_string "  ds: "; print_delta ctxts.d.s;
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
            f, ctxts, Spawn(cRec, Var(f.xRecLam), [], Fwd(cRec, c, t))
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
    match goal with 
    | TArrow(t1, t2) ->
        let x = fresh_id() in
        begin match get_TArrow_return_type goal with
        | TProcess(_, STRec _) ->
            if not (List.mem_assoc f.xRecLam ctxts.g.a || List.mem_assoc f.xRecLam ctxts.g.s) then
                let f = { f with xRecLam = fresh_id() } in
                let ctxts1 = append_bindings_psi ctxts [(x, t1); (f.xRecLam, goal)] in
                let f, ctxts', e = invertRightF f ctxts1 c t2 in
                f, ctxts', LetRec(f.xRecLam, Lam(x, t1, e))
            else
                let ctxts1 = append_bindings_psi ctxts [(x, t1)] in
                let f, ctxts', e = invertRightF f ctxts1 c t2 in
                f, ctxts, Lam(x, t1, e)
        | _ ->
            let ctxts1 = append_bindings_psi ctxts [(x, t1)] in
            let f, ctxts', e = invertRightF f ctxts1 c t2 in
            f, ctxts', Lam(x, t1, e)
        end
    | TProcess(insl, outs) ->
        let incsl = List.map (fun (s) -> (fresh_channel(), s)) insl in
        let outc = fresh_channel() in
        let ctxts1 = append_bindings_delta ctxts incsl in
        let f, ctxts', e = invertRightS f ctxts1 c outs in
        f, ctxts', Process(outc, e, outs, incsl)
    | _ -> invertLeftF f ctxts c goal

and invertLeftS f ctxts c goal =
    print_endline ("invertLeft: " ^ tyS_to_string goal);
    print_string "  da: "; print_delta ctxts.d.a;
    print_string "  ds: "; print_delta ctxts.d.s;
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
    match ctxts.p.a with
    | (x, t)::pa' ->
        begin match t with
        | _ -> raise (Fail("invertLeftF: somehow a sync type wound up in async context"))
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
    print_string "  ps: "; print_psi ps';
    match ps' with
    | [] -> focusRightF f ctxts c goal
    | (xFoc, tFoc)::ps'' -> try 
            if is_tyF_left_async goal then focusRightF f ctxts c goal
            else focusLeftF f ctxts xFoc tFoc c goal
        with Fail _ -> focusDecideF f ctxts ps'' c goal

and focusRightS f ctxts c goal =
    print_endline ("focusRight: " ^ tyS_to_string goal);
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
    match goal with
    | TInt -> f, ctxts, Int(1)
    | TBool -> f, ctxts, Bool(true)
    | _ -> invertRightF f ctxts c goal

and focusLeftS f ctxts xFocus tFocus c goal =
    print_endline ("focusLeftS: " ^ tyS_to_string tFocus);
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
    match tFocus with
    | TArrow(t1, t2) ->
        let y = fresh_id() in
        let f, ctxts', e2 = focusLeftF f ctxts y t2 c goal in
        let f, ctxts'', e1 = invertRightF f ctxts c t1 in
        f, ctxts'', subst e2 y (App(Var(xFocus), e1))
    | TInt | TBool -> 
        if tFocus = goal then f, ctxts, Var(xFocus)
        else raise (Fail "tFocus != goal")
    | _ -> raise (Fail("focusLeftF: somehow foc type is left async: " ^ tyF_to_string tFocus))

let synth goal = 
    let f : flags = { isUnfolded = false ; xRecLam = "" } in
    let g : gamma = { a = []; s = [] } in
    let p : psi = { a = []; s = [] } in
    let d : delta = { a = []; s = [] } in
    let ctxts : contexts = { g = g; p = p; d = d } in
    let f, ctxts', e = invertRightF f ctxts (fresh_channel()) goal in
    if List.is_empty ctxts'.d.a && List.is_empty ctxts'.d.s then e
    else raise (Fail("Synthesized expression did not use all linear resources:\n  da: " ^ delta_to_string ctxts'.d.a ^ "\n  ds: " ^ delta_to_string ctxts'.d.s ^ "\n"))

end;;

(* Running stuff *)

let synthType = 
    (*TProcess([], STSendF(TInt, STSendF(TBool, STUnit)))*)
    TArrow(TInt, TProcess([], STRec("t", STSendF(TInt, STRecVar("t")))))
in
let synthExp = Sessynth.synth synthType in
print_endline "" ; print_endline (expF_to_string synthExp)

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