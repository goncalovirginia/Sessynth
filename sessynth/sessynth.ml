exception Fail of string

(* Types and terms *)

type id = string

type tyF = (* ordinary functional types (F) *)
    | TAtom of id (* id encoding any atomic type (int, bool, string, etc) *)
    | TExponential of tyF (* !F *)
    | TArrow of tyF * tyF (* F1 -> F2 *)
    | TProcess of tyS list * tyS (* {S1, ..., Sn |- P :: c : S} *)

and tyS = (* channel/session types (S) *)
    | STSendF of tyF * tyS (* F ∧ S *)
    | STRecvF of tyF * tyS (* F ⊃ S *)
    | STSendS of tyS * tyS (* S1 ⊗ S2 *)
    | STRecvS of tyS * tyS (* S1 -o S2 *)
    | STUnit (* 1 *)
    | STExtChoice of (id * tyS) list (* &{ l1:S1, ..., ln:Sn } *)
    | STIntChoice of (id * tyS) list (* ⊕{ l1:S1, ..., ln:Sn } *)
    | STRec of id * tyS (* mu x . S *)
    | STVarRec of id (* x *)

type tySDeclr = (* session type declaration/binding onto an id *)
    | STDeclr of id * tyS (* stype x = S *)

type expF = (* functional terms (M) *)
    | Var of id (* x *)
    | Let of id * expF * expF (* let x = M1 in M2 *)
    | Exponential of expF (* !M *)
    | LetExponential of id * expF * expF (* let !x = M1 in M2 *)
    | Lam of id * tyF * expF (* x:F -> M *)
    | App of expF  * expF (* (M1) M2 *)
    | Process of id * expP * tyS * (id * tyS) list (* c <- {P :: c : S} <- [c1:S1; ...; cn:Sn] (opaque functional value, P not evaluated) *)
    | ExecProcess of expF (* M *)
    | LetRec of id * expF * expF (* let rec x = M1 in M2 *)

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
    
let fresh_id = 
    let unique = ref (-1) in
    fun () -> (incr unique; "x" ^ (string_of_int !unique))

let fresh_channel = 
    let unique = ref (-1) in
    fun () -> (incr unique; "c" ^ (string_of_int !unique))

let rec label_tyS_list_to_string xtl =
    match xtl with
    | (x, t)::xtl' -> x ^ ":" ^ tyS_to_string t ^ ", " ^ label_tyS_list_to_string xtl' 
    | [] -> ""

and tyF_to_string t =
    match t with 
    | TAtom x -> x
    | TExponential(t) -> "!" ^ tyF_to_string t
    | TArrow(t1, t2) -> ""
    | TProcess(tl, t) -> ""
    | _ -> ""

and tyS_to_string t =
    match t with 
    | STUnit -> "1"
    | STExtChoice(xtl) -> "&{" ^ label_tyS_list_to_string xtl ^ "}"
    | STIntChoice(xtl) -> "⊕{" ^ label_tyS_list_to_string xtl ^ "}"
    | _ -> ""

let rec expF_to_string e =
    match e with 
    | Var x -> x
    | Let(x, e1, e2) -> "let" ^ x ^ " = " ^ expF_to_string e1 ^ " in " ^expF_to_string e2
    | Exponential e -> "!" ^ expF_to_string e
    | LetExponential(x, e1, e2) -> "let !" ^ x ^ " = " ^ expF_to_string e1 ^ " in " ^expF_to_string e2
    | Lam(x, t, e) -> x ^ ":" ^ tyF_to_string t ^ " -o " ^ expF_to_string e 
    | App(e1, e2) -> "(" ^ expF_to_string e1 ^ ") " ^ expF_to_string e2
    | _ -> ""

let rec expP_to_string e =
    match e with 
    | Var x -> x
    | Let(x, e1, e2) -> "let" ^ x ^ " = " ^ expF_to_string e1 ^ " in " ^expF_to_string e2
    | Exponential e -> "!" ^ expF_to_string e
    | LetExponential(x, e1, e2) -> "let !" ^ x ^ " = " ^ expF_to_string e1 ^ " in " ^expF_to_string e2
    | Lam(x, t, e) -> x ^ ":" ^ tyF_to_string t ^ " -o " ^ expF_to_string e 
    | App(e1, e2) -> "(" ^ expF_to_string e1 ^ ") " ^ expF_to_string e2
    | _ -> ""

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
    | TExponential _ -> true
    | _ -> false

let is_tyS_left_async t =
    match t with
    | STSendF _ | STSendS _ | STUnit | STIntChoice _ -> true
    | _ -> false

let rec append_bindings context bindings is_left_async_func =
    let contexta, contexts = context in
    match bindings with
    | (x, t)::bindings' ->
        if is_left_async_func t then append_bindings ((x, t)::contexta, contexts) bindings' is_left_async_func
        else append_bindings (contexta, (x, t)::contexts) bindings' is_left_async_func
    | [] -> context

let rec append_bindings_psi p bindings = 
    append_bindings p bindings is_tyF_left_async

let rec append_bindings_delta d bindings =
    append_bindings d bindings is_tyS_left_async

let rec deltas_are_equal inversions prevDelta = 
    let daPrev, dsPrev = prevDelta in
    match inversions with
    | (_, _, da', ds', _)::inversions' -> List.equal (=) daPrev da' && List.equal (=) dsPrev ds' && deltas_are_equal inversions' (da', ds')
    | [] -> true

let rec build_label_expP_list labelsesslist inversions =
    match labelsesslist, inversions with
    | (l, _)::labelsesslist', (_, _, _, _, expP)::inversions' -> (l, expP)::build_label_expP_list labelsesslist' inversions'
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

let rec unfold_TArrow t =
    match t with
    | TArrow(t1, t2) -> unfold_TArrow t2
    | _ -> t

(* Focused type-driven synthesizer *)

module Sessynth = struct 

(* gamma; psi; delta-async; delta-sync |- P :: c : goal *)
let rec invertRightS g p da ds c goal = 
    print_endline ("invertRightS: " ^ tyS_to_string goal);
    print_string "  ds: "; print_delta da;
    print_string "  da: "; print_delta ds;
    match goal with 
    | STRecvF(t1, t2) ->
        let x = fresh_id() in
        let p1 = append_bindings_psi p [(x, t1)] in
        let g, p', da', ds', e = invertRightS g p1 da ds c t2 in
        assert (List.assoc_opt x da' = None && List.assoc_opt x ds' = None);
        g, p', da', ds', RecvF(x, c, e)
    | STRecvS(t1, t2) -> 
        let x = fresh_id() in
        let da1, ds1 = append_bindings_delta (da, ds) [(x, t1)] in
        let g, p', da', ds', e = invertRightS g p da1 ds1 c t2 in
        assert (List.assoc_opt x da' = None && List.assoc_opt x ds' = None);
        g, p', da', ds', RecvS(x, c, e)
    | STExtChoice(labelsesslist) ->
        let inversions = List.map (fun (l, s) -> invertRightS g p da ds c s) labelsesslist in
        let g1, p1, a1, s1, _ = List.hd inversions in
        assert (deltas_are_equal (List.tl inversions) (a1, s1));
        let labelprocesslist = build_label_expP_list labelsesslist inversions in
        g1, p1, a1, s1, Choice(c, labelprocesslist)
    | STRec(x, t) ->
        let expSpawn = spawn_process_rec in
        g, p, da, ds, expSpawn
    | _ -> invertLeftS g p da ds c goal

(*
nats : int -> {rec t. int * t }
let nats = fun x ->
  c <- { 
    // nats: int -> {rec t. int * t} , x:int |- c: rec t. int * t
    // nats: int -> {rec t. int * t} , x:int |- c: int * (rec t. int * t)
    send c 0 ;  
    d <- spawn (nats 0); // nats: int -> {rec t. int * t} , x:int ; empty |- c: t // c: rec t. int * t
	fwd d c // nats: int -> {rec t. int * t} , x:int ; d:rec t.int*t |- c: t // c:rec t . int*t
	}
*)

and invertRightF g p d c goal =
    print_endline ("invertRightF: " ^ tyF_to_string goal);
    print_string "  ds: "; print_psi (fst p);
    print_string "  da: "; print_psi (snd p);
    match goal with 
    | TArrow(t1, t2) ->
        let x = fresh_id() in
        let p1 = append_bindings_psi p [(x, t1)] in
        let g, p', da', ds', e = invertRightF g p1 d c t2 in
        g, p', da', ds', Lam(x, t1, e)
    | TProcess(insl, outs) ->
        let incsl = List.map (fun (s) -> (fresh_channel(), s)) insl in
        let outc = fresh_channel() in
        let da1, ds1 = append_bindings_delta d incsl in
        let g', p', da', ds', e = invertRightS g p da1 ds1 c outs in
        g', p', da', ds', Process(outc, e, outs, incsl)
    | _ -> invertLeftF g p d c goal

and invertLeftS g p da ds c goal =
    print_endline ("invertLeft: " ^ tyS_to_string goal);
    print_string "  da: "; print_delta da;
    print_string "  ds: "; print_delta ds;
    match da with
    | (x, t)::da' ->
        begin match t with 
        | STSendF(t1, t2) ->
            let x, c' = fresh_id(), fresh_channel() in
            let p1 = append_bindings_psi p [(x, t1)] in
            let da1, ds1 = append_bindings_delta (da', ds) [(c, t2)] in
            let g, p', da', ds', e = invertLeftS g p1 da1 ds1 c' goal in
            assert (List.assoc_opt x da' = None && List.assoc_opt x ds' = None && List.assoc_opt c da' = None && List.assoc_opt c ds' = None);
            g, p', da', ds', RecvF(x, c, e)
        | STSendS(t1, t2) ->
            let x, c' = fresh_channel(), fresh_channel() in
            let da1, ds1 = append_bindings_delta (da', ds) [(x, t1); (c, t2)] in
            let g, p', da', ds', e = invertLeftS g p da1 ds1 c' goal in
            assert (List.assoc_opt x da' = None && List.assoc_opt x ds' = None && List.assoc_opt c da' = None && List.assoc_opt c ds' = None);
            g, p', da', ds', RecvS(x, c, e)
        | STUnit -> 
            let c' = fresh_channel() in
            let g, p', da', ds', e = invertLeftS g p da' ds c' goal in
            g, p', da', ds', Wait(c, e)
        | STIntChoice(labelsesslist) ->
            let inversions = List.map (fun (l, s) -> 
                let xn = fresh_id() in
                let da1, ds1 = append_bindings_delta (da', ds) [(xn, s)] in
                let g, p', da', ds', e = invertLeftS g p da1 ds1 c goal in
                assert (List.assoc_opt xn da' = None && List.assoc_opt xn ds' = None);
                g, p', da', ds', e
            ) labelsesslist in
            let g1, p1, da1, ds1, _ = List.hd inversions in
            assert (deltas_are_equal (List.tl inversions) (da1, ds1));
            let labelprocesslist = build_label_expP_list labelsesslist inversions in
            g1, p1, da1, ds1, Choice(x, labelprocesslist)
        | _ -> raise (Fail("invertLeft: somehow a sync type wound up in async context"))
        end
    | [] -> focusDecideS g p ds ds c goal

and invertLeftF g p d c goal =
    let pa, ps = p in
    match pa with
    | (x, t)::pa' ->
        begin match t with
        | TExponential t1 ->
            let x1 = fresh_id() in
            let g', p', da', ds', e = invertLeftF ((x1, t)::g) p d c t1 in
            g', p', da', ds', LetExponential(x1, Var(x), e)
        | _ -> raise (Fail("invertLeftF: somehow a sync type wound up in async context"))
        end
    | [] -> focusDecideF g ps ps d c goal 

and focusDecideS g p ds dsOriginal c goal = 
    print_endline ("focusDecideS: " ^ tyS_to_string goal);
    print_string "  ds: "; print_delta ds;
    match ds with
    | [] -> raise (Fail "focusDecide: empty sync context")
    | (xFocus, tFocus)::ds' -> try 
            if is_tyS_left_async goal then focusRightS g p dsOriginal c goal
            else focusLeftS g p dsOriginal xFocus tFocus c goal
        with Fail _ -> focusDecideS g p ds' dsOriginal c goal

and focusDecideF g ps psOriginal d c goal = 
    print_endline ("focusDecideF: " ^ tyF_to_string goal);
    print_string "  ps: "; print_psi ps;
    match ps with
    | [] -> raise (Fail "focusDecide: empty sync context")
    | (xFocus, tFocus)::ps' -> try 
            if is_tyF_left_async goal then focusRightF g psOriginal d c goal
            else focusLeftF g psOriginal xFocus tFocus d c goal
        with Fail _ -> focusDecideF g ps' psOriginal d c goal

and focusRightS g p ds c goal =
    print_endline ("focusRight: " ^ tyS_to_string goal);
    match goal with 
    | STSendF(t1, t2) ->
        let g, p', da', ds', e1 = focusDecideF g (snd p) (snd p) ([], ds) c t1 in
        let g, p'', da'', ds'', e2 = focusRightS g p' ds' c t2 in
        g, p'', da'', ds'', SendF(c, e1 , e2)
    | STSendS(t1, t2) -> 
        let y = fresh_channel() in
        let g, p', da', ds', e1 = focusRightS g p ds y t1 in
        let g, p'', da'', ds'', e2 = focusRightS g p' ds' c t2 in
        g, p'', da'', ds'', SendS(c, y, e1 , e2)
    | STUnit ->
        g, p, [], ds, Close(c)
    | STIntChoice(labelsesslist) -> 
        let rec iter_labels labelsesslist =
            match labelsesslist with
            | (l, s)::labelsesslist' ->
                begin 
                    try begin
                        let g, p', da', ds', e1 = focusRightS g p ds c s in
                        g, p', da', ds', ChoiceSelect(c, l, e1)
                    end
                    with Fail _ -> try begin
                        iter_labels labelsesslist'
                    end
                    with Fail m -> raise (Fail m)
                end
            | [] -> raise (Fail "focusLeft: TExtChoice has no valid label-sess option")
        in iter_labels labelsesslist
    | _ -> invertRightS g p [] ds c goal (* goal is not right sync, therefore switch back to inversion phase *)

and focusRightF g ps d c goal =
    match goal with
    | TExponential t ->
        let g', p', da', ds', e = invertRightF g ([], ps) d c goal in
        g', p', da', ds', Exponential(e)
    | _ -> invertRightF g ([], ps) d c goal

and focusLeftS g p ds xFocus tFocus c goal =
    print_endline ("focusLeftS: " ^ tyS_to_string tFocus);
    match tFocus with
    | STRecvF(t1, t2) ->
        let g, p', da', ds', e2 = focusLeftS g p ds xFocus t2 c goal in
        let g, p'', da'', ds'', e1 = invertRightF g p' (da', ds') c t1 in
        g, p'', da'', ds'', SendF(xFocus, e1, e2) 
    | STRecvS(t1, t2) -> 
        let y = fresh_id() in
        let g, p', da', ds', e2 = focusLeftS g p ds xFocus t2 c goal in
        let g, p'', da'', ds'', e1 = invertRightS g p' da' ds' y t1 in
        g, p'', da'', ds'', SendS(xFocus, y, e1, e2) 
    | STExtChoice(labelsesslist) -> 
        let rec iter_labels labelsesslist =
            match labelsesslist with
            | (l, s)::labelsesslist' ->
                begin 
                    try begin
                        let g, p', da', ds', e = focusLeftS g p ds xFocus s c goal in
                        g, p', da', ds', ChoiceSelect(xFocus, l, e)
                    end
                    with Fail _ -> try begin
                        iter_labels labelsesslist'
                    end
                    with Fail m -> raise (Fail m)
                end
            | [] -> raise (Fail "focusLeft: TExtChoice has no valid label-sess option")
        in iter_labels labelsesslist
    | STRec(x, t) -> (* TODO *)
        g, p, [], ds, Close("TODO")
    | _ -> raise (Fail("focusLeftS: somehow foc type is left async: " ^ tyS_to_string tFocus))

and focusLeftF g ps xFocus tFocus d c goal =
    match tFocus with
    | TArrow(t1, t2) ->
        let y = fresh_id() in
        let g', p', da', ds', e2 = focusLeftF g ps y t2 d c goal in
        let g'', p'', da'', ds'', e1 = invertRightF g ([], ps) d c t1 in
        g'', p'', da'', ds'', subst e2 y (App(Var(xFocus), e1))
    | TAtom _ -> 
        if tFocus = goal then g, ([], ps), fst d, snd d, Var(xFocus)
        else raise (Fail "tFocus != goal")
    | _ -> raise (Fail("focusLeftF: somehow foc type is left async: " ^ tyF_to_string tFocus))

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

and fwd_channel d c1 c2 =
    let d', s1 = consume_channel d c1 in
    d', Fwd(c1, c2, s1)

let synth goal = 
    let g, p', da', ds', e = invertRightS [] ([], []) [] [] (fresh_channel()) goal in
    if List.is_empty da' && List.is_empty ds' then e
    else raise (Fail("Synthesized expression did not use all linear resources:\n  da: " ^ delta_to_string da' ^ "\n  ds: " ^ delta_to_string ds' ^ "\n"))

end;;

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

(* Running stuff *)

(*
let synthType = 
    (*TArrow(TAddConjPair(TAtom("bool"), TAtom("int")), TAtom("int"))*)
    TRecvS(TAtom("float"), TRecvS(TSendS(TAtom("int"), TAtom("bool")), TSendS(TAtom("bool"), TSendS(TAtom("float"), TAtom("int")))))
    (*TArrow(TAtom("int"), TAddDisjCase(TAtom("int"), TAtom("bool")))*)
in
let exp = Sessynth.synth synthType in
print_endline "" ; print_endline (exp_to_string exp)
*)
