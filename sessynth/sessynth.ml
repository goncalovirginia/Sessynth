exception Fail of string

(* Types and terms *)

type id = string

type tyF = (* ordinary functional types (F) *)
    | TAtom of id (* id encoding any atomic type *)
    | TExponential of tyF (* !F *)
    | TArrow of tyF * tyF (* F1 -> F2 *)
    | TProcess of (id * tyS) list * tyS (* {c1:S1, ..., cn:Sn |- P :: c : S} *)

and tyS = (* channel/session types (S) *)
    | TSendF of tyF * tyS (* F ∧ S *)
    | TRecvF of tyF * tyS (* F ⊃ S *)
    | TSendS of tyS * tyS (* S1 ⊗ S2 *)
    | TRecvS of tyS * tyS (* S1 -o S2 *)
    | TUnit (* 1 *)
    | TExtChoice of (id * tyS) list (* &{ l1:S1, ..., ln:Sn } *)
    | TIntChoice of (id * tyS) list (* ⊕{ l1:S1, ..., ln:Sn } *)

type tySDeclr = (* session type declaration/binding onto an id *)
    | TSDeclr of id * tyS (* stype x = S *)

type expF = (* functional terms (M) *)
    | Var of id (* x *)
    | Let of id * expF * expF (* let x = M1 in M2 *)
    | Exponential of expF (* !M *)
    | LetExponential of id * expF * expF (* let !x = M1 in M2 *)
    | Lam of id * tyF * expF (* x:F -> M *)
    | App of expF  * expF (* (M1) M2 *)
    | Process of id * expP * tyS * (id * tyS) list (* c <- {P :: c : S} <- [c1:S1; ...; cn:Sn] (opaque functional value, P not evaluated) *)

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
    | Spawn of id * expF * id list * expP (* c <- spawn M ci; P *)

(* Auxiliary functions *)

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
    | TUnit -> "1"
    | TExtChoice(xtl) -> "&{" ^ label_tyS_list_to_string xtl ^ "}"
    | TIntChoice(xtl) -> "⊕{" ^ label_tyS_list_to_string xtl ^ "}"
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

let rec print_delta c = print_endline (delta_to_string c)

let is_tyF_left_async t = 
    match t with
    | TExponential _ -> true
    | _ -> false

let is_tyS_left_async t =
    match t with
    | TSendS _ | TUnit | TIntChoice _ -> true
    | _ -> false

let rec append_bindings da ds bindings =
    match bindings with
    | (x, t)::bindings' ->
        if is_tyS_left_async t then append_bindings ((x, t)::da) ds bindings'
        else append_bindings da ((x, t)::ds) bindings'
    | [] -> da, ds

let rec deltas_are_equal inversions prevDelta = 
    let daPrev, dsPrev = prevDelta in
    match inversions with
    | (_, _, da', ds', _)::inversions' -> List.equal (=) daPrev da' && List.equal (=) dsPrev ds' && deltas_are_equal inversions' (da', ds')
    | [] -> true

let rec build_label_expP_list labelsesslist inversions =
    match labelsesslist, inversions with
    | (l, _)::labelsesslist', (_, _, _, _, expP)::inversions' -> (l, expP)::build_label_expP_list labelsesslist' inversions'
    | [], [] | [], _ | _, [] -> []

(* Focused type-driven synthesizer *)

module Sessynth = struct 

(* gamma; psi; delta-async; delta-sync |- P :: c : goal *)
let rec invertRightS g p da ds c goal = 
    print_endline ("invertRight: " ^ tyS_to_string goal);
    print_string "  ds: "; print_delta da;
    print_string "  da: "; print_delta ds;
    match goal with 
    | TRecvF(t1, t2) ->
        let x = fresh_id() in
        let p1 = (x, t1)::p in
        let g, p', da', ds', e = invertRightS g p1 da ds c t2 in
        assert (List.assoc_opt x da' = None && List.assoc_opt x ds' = None);
        g, p', da', ds', RecvF(x, c, e)
    | TRecvS(t1, t2) -> 
        let x = fresh_id() in
        let da1, ds1 = append_bindings da ds [(x, t1)] in
        let g, p', da', ds', e = invertRightS g p da1 ds1 c t2 in
        assert (List.assoc_opt x da' = None && List.assoc_opt x ds' = None);
        g, p', da', ds', RecvS(x, c, e)
    | TExtChoice(labelsesslist) ->
        let inversions = List.map (fun (l, s) -> invertRightS g p da ds c s) labelsesslist in
        let g1, p1, a1, s1, _ = List.hd inversions in
        assert (deltas_are_equal (List.tl inversions) (a1, s1));
        let labelprocesslist = build_label_expP_list labelsesslist inversions in
        g1, p1, a1, s1, Choice(c, labelprocesslist)
    | _ -> invertLeftS g p da ds c goal

and invertRightF g p goal =
    match goal with 
    | TArrow _
    | _ -> invertLeftF g p goal

and invertLeftS g p da ds c goal =
    print_endline ("invertLeft: " ^ tyS_to_string goal);
    print_string "  da: "; print_delta da;
    print_string "  ds: "; print_delta ds;
    match da with
    | (x, t)::da' ->
        begin match t with 
        | TSendS(t1, t2) ->
            let x, c' = fresh_id(), fresh_channel() in
            let da1, ds1 = append_bindings da' ds [(x, t1); (c, t2)] in
            let g, p', da', ds', e = invertLeftS g p da1 ds1 c' goal in
            assert (List.assoc_opt x da' = None && List.assoc_opt x ds' = None && List.assoc_opt c da' = None && List.assoc_opt c ds' = None);
            g, p', da', ds', RecvS(x, c, e)
        | TUnit -> 
            let c' = fresh_channel() in
            let g, p', da', ds', e = invertLeftS g p da' ds c' goal in
            g, p', da', ds', Wait(c, e)
        | TIntChoice(labelsesslist) ->
            let inversions = List.map (fun (l, s) -> 
                let xn = fresh_id() in
                let da1, ds1 = append_bindings da' ds [(xn, s)] in
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

and invertLeftF g p goal =
    match p with
    | (x, t)::p' ->
        begin match t with
        | TExponential t ->
            let x1 = fresh_id() in
            let g', p', e = invertLeftF ((x1, t)::g) p t in
            g', p', LetExponential(x1, Var(x), e)
        | _ -> g, p, Var("TODO")
        end
    | [] -> focusRightF g p goal (* TODO, SHOULD NOT INVERT LEFT F INFINITELY *)

and focusDecideS g p ds dsOriginal c goal = 
    print_endline ("focusDecide: " ^ tyS_to_string goal);
    print_string "  ds: "; print_delta ds;
    match ds with
    | [] -> raise (Fail "focusDecide: empty sync context")
    | (xFocus, tFocus)::ds' -> try 
            if is_tyS_left_async goal then focusRightS g p dsOriginal c goal
            else focusLeftS g p dsOriginal xFocus tFocus c goal
        with Fail _ -> focusDecideS g p ds' dsOriginal c goal

and focusRightS g p ds c goal =
    print_endline ("focusRight: " ^ tyS_to_string goal);
    match goal with 
    | TSendS(t1, t2) -> 
        let y = fresh_channel() in
        let g, p', da', ds', e1 = focusRightS g p ds y t1 in
        let g, p'', da'', ds'', e2 = focusRightS g p' ds' c t2 in
        g, p'', da'', ds'', SendS(c, y, e1 , e2)
    | TUnit ->
        g, p, [], ds, Close(c)
    | TIntChoice(labelsesslist) -> 
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

and focusRightF g p goal =
    match goal with
    | TExponential t ->
        let g', p', e = invertRightF g p goal in
        g', p', Exponential(e)
    | _ -> invertRightF g p goal

and focusLeftS g p ds xFocus tFocus c goal =
    print_endline ("focusLeft: " ^ tyS_to_string tFocus);
    match tFocus with
    | TRecvS(t1, t2) -> 
        begin try
            let y = fresh_id() in
            let g, p', da', ds', e2 = focusLeftS g p ds xFocus t2 c goal in
            let g, p'', da'', ds'', e1 = invertRightS g p' da' ds' y t1 in
            g, p'', da'', ds'', SendS(xFocus, y, e1, e2) 
        with Fail m -> raise (Fail m)
        end
    | TExtChoice(labelsesslist) -> 
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
    | _ -> raise (Fail("focusLeftS: somehow foc type is left async: " ^ tyS_to_string tFocus))

and focusLeftF g p xFocus tFocus goal =
    match tFocus with
    | TAtom _ -> 
        if tFocus = goal then g, [], List.remove_assoc xFocus p, Var(xFocus)
        else raise (Fail "tFocus != goal")
    | TArrow _
    | _ -> raise (Fail("focusLeftF: somehow foc type is left async: " ^ tyF_to_string tFocus))

    let synth goal = 
        let g, p', da', ds', e = invertRightS [] [] [] [] (fresh_channel()) goal in
        if List.is_empty da' && List.is_empty ds' then e
        else raise (Fail("Synthesized expression did not use all linear resources:\n  async: " ^ delta_to_string da' ^ "\n  sync: " ^ delta_to_string ds' ^ "\n"))

end;;

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
