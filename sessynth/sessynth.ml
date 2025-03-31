exception Fail of string

(* Types and terms *)

type id = string

type ty = 
    (* ordinary functional types (F) *)
    | TAtom of id (* id encoding any atomic type *)
    | TExponential of ty (* !F *)
    | TArrow of ty * ty (* F1 -> F2 *)
    | TProcess of (id * ty) list * ty (* {c1:S1, ..., cn:Sn |- P :: c : S} *)
    (* channel/session types (S) *)
    | TSendF of ty * ty (* F ∧ S *)
    | TRecvF of ty * ty (* F ⊃ S *)
    | TSendS of ty * ty (* S1 ⊗ S2 *)
    | TRecvS of ty * ty (* S1 -o S2 *)
    | TUnit (* 1 *)
    | TExtChoice of (id * ty) list (* &{ l1:S1, ..., ln:Sn } *)
    | TIntChoice of (id * ty) list (* ⊕{ l1:S1, ..., ln:Sn } *)
    (* session type declaration/binding onto an id *)
    | TSDeclr of id * ty (* stype x = S *)

type exp = 
    (* functional terms (M) *)
    | Var of id (* x *)
    | Let of id * exp * exp (* let x = M1 in M2 *)
    | Exponential of exp (* !M *)
    | LetExponential of id * exp * exp (* let !x = M1 in M2 *)
    | Lam of id * ty * exp (* x:F -> M *)
    | App of exp  * exp (* (M1) M2 *)
    | Process of id * exp * ty * (id * ty) list (* c <- {P :: c : S} <- [c1:S1; ...; cn:Sn] (opaque functional value, P not evaluated) *)
    (* process terms (P) *)
    | SendF of id * exp * exp (* send c M; P : F ∧ S *)
    | RecvF of id * id * exp (* x:F <- recv c; P : F ⊃ S *)
    | SendS of id * id * exp * exp (* send c1 (c2 P2); P : S1 ⊗ S2 *)
    | RecvS of id * id * exp (* x:S <- recv c; P : S1 -o S2 *)
    | Close of id (* close c : 1 *)
    | Wait of id * exp (* wait c; P *)
    | Fwd of id * id * tyS (* fwd c1 c2 :: c2 : S1 *)
    | Choice of id * (id * exp) list (* case c of li:Pi :: c : &{ l1:S1, ..., ln:Sn } *)
    | ChoiceSelect of id * id * exp (* c.l; P :: c : ⊕{ l1:S1, ..., ln:Sn } *)
    | Spawn of id * exp * id list * exp (* c <- spawn M ci; P *)

(*
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
*)

(* Auxiliary functions *)

let fresh_id = 
    let unique = ref (-1) in
    fun () -> (incr unique; "x" ^ (string_of_int !unique))

let fresh_channel = 
    let unique = ref (-1) in
    fun () -> (incr unique; "c" ^ (string_of_int !unique))

let rec label_type_list_to_string xtl =
    match xtl with
    | (x, t)::xtl' -> x ^ ":" ^ type_to_string t ^ ", " ^ label_type_list_to_string xtl' 
    | [] -> ""

and type_to_string t =
    match t with 
    | TAtom x -> x
    | TExponential(t) -> "!" ^ type_to_string t
    | TArrow(t1, t2) -> ""
    | TProcess(tl, t) -> ""
    | TUnit -> "1"
    | TExtChoice(xtl) -> "&{" ^ label_type_list_to_string xtl ^ "}"
    | TIntChoice(xtl) -> "⊕{" ^ label_type_list_to_string xtl ^ "}"
    | TSDeclr(x, t) -> ""
    | _ -> ""

let rec exp_to_string e = (* process terms (P) *)
    match e with 
    (* functional terms (M) *)
    | Var x -> x
    | Let(x, e1, e2) -> "let" ^ x ^ " = " ^ exp_to_string e1 ^ " in " ^exp_to_string e2
    | Exponential e -> "!" ^ exp_to_string e
    | LetExponential(x, e1, e2) -> "let !" ^ x ^ " = " ^ exp_to_string e1 ^ " in " ^exp_to_string e2
    | Lam(x, t, e) -> x ^ ":" ^ type_to_string t ^ " -o " ^ exp_to_string e 
    | App(e1, e2) -> "(" ^ exp_to_string e1 ^ ") " ^ exp_to_string e2
    (* process terms (P) *)
    | Close(c) -> "1"
    | Wait(c, e) -> ""
    | Spawn(c, e1, cl, e2) -> ""
    | _ -> ""

let rec context_to_string c = 
    match c with
    | [] -> "";
    | (x, t)::c' -> x ^ ":" ^ (type_to_string t) ^ "; " ^ context_to_string c'

let rec print_context c = print_endline (context_to_string c)

let is_left_async t = 
    match t with
    | TSendS _ | TUnit | TIntChoice _ | TExponential _ -> true
    | _ -> false

let rec append_bindings async sync bindings =
    match bindings with
    | (x, t)::bindings' ->
        if is_left_async t then append_bindings ((x, t)::async) sync bindings'
        else append_bindings async ((x, t)::sync) bindings'
    | [] -> async, sync

let rec deltas_are_equal inversions prevDelta = 
    let daPrev, dsPrev = prevDelta in
    match inversions with
    | (_, da', ds', _)::inversions' -> List.equal (=) daPrev da' && List.equal (=) dsPrev ds' && deltas_are_equal inversions' (da', ds')
    | [] -> true

let rec build_label_expP_list labelsesslist inversions =
    match labelsesslist, inversions with
    | (l, _)::labelsesslist', (_, _, _, expP)::inversions' -> (l, expP)::build_label_expP_list labelsesslist' inversions'
    | [], [] | [], _ | _, [] -> []

(* Focused type-driven synthesizer *)

module Sessynth = struct 

let rec invertRight g da ds c goal = 
    print_endline ("invertRight: " ^ type_to_string goal);
    print_string "  ds: "; print_context da;
    print_string "  da: "; print_context ds;
    match goal with 
    | TRecvS(t1, t2) -> 
        let x = fresh_id() in
        let da1, ds1 = append_bindings da ds [(x, t1)] in
        let g, da', ds', e = invertRight g da1 ds1 c t2 in
        assert (List.assoc_opt x da' = None && List.assoc_opt x ds' = None);
        g, da', ds', RecvS(x, c, e)
    | TExtChoice(labelsesslist) ->
        let inversions = List.map (fun (l, s) -> invertRight g da ds c s) labelsesslist in
        let g1, a1, s1, _ = List.hd inversions in
        assert (deltas_are_equal (List.tl inversions) (a1, s1));
        let labelprocesslist = build_label_expP_list labelsesslist inversions in
        g1, a1, s1, Choice(c, labelprocesslist)
    | _ -> invertLeft g da ds c goal

and invertLeft g da ds c goal =
    print_endline ("invertLeft: " ^ type_to_string goal);
    print_string "  da: "; print_context da;
    print_string "  ds: "; print_context ds;
    match da with
    | (x, t)::da' ->
        begin match t with 
        | TSendS(t1, t2) ->
            let x, c' = fresh_id(), fresh_channel() in
            let da1, ds1 = append_bindings da' ds [(x, t1); (c, t2)] in
            let g, da', ds', e = invertLeft g da1 ds1 c' goal in
            assert (List.assoc_opt x da' = None && List.assoc_opt x ds' = None && List.assoc_opt c da' = None && List.assoc_opt c ds' = None);
            g, da', ds', RecvS(x, c, e)
        | TUnit -> 
            let c' = fresh_channel() in
            let g, da', ds', e = invertLeft g da' ds c' goal in
            g, da', ds', Wait(c, e)
        | TIntChoice(labelsesslist) ->
            let inversions = List.map (fun (l, s) -> 
                let xn = fresh_id() in
                let da1, ds1 = append_bindings da' ds [(xn, s)] in
                let g, da', ds', e = invertLeft g da1 ds1 c goal in
                assert (List.assoc_opt xn da' = None && List.assoc_opt xn ds' = None);
                g, da', ds', e
            ) labelsesslist in
            let g1, da1, ds1, _ = List.hd inversions in
            assert (deltas_are_equal (List.tl inversions) (da1, ds1));
            let labelprocesslist = build_label_expP_list labelsesslist inversions in
            g1, da1, ds1, Choice(x, labelprocesslist)
        | TExponential t ->
            let x1 = fresh_id() in
            let g', da', ds', e = invertLeft ((x1, t)::g) da' ds c t in
            g', da', ds', LetExponential(x1, Var(x), e)
        | _ -> raise (Fail("invertLeft: somehow a sync type wound up in async context"))
        end
    | [] -> focusDecide g ds ds c goal

and focusDecide g ds dsOriginal c goal = 
    print_endline ("focusDecide: " ^ type_to_string goal);
    print_string "  ds: "; print_context ds;
    match ds with
    | [] -> raise (Fail "focusDecide: empty sync context")
    | (xFocus, tFocus)::ds' -> try 
            if is_left_async goal then focusRight g dsOriginal c goal
            else focusLeft g dsOriginal xFocus tFocus c goal
        with Fail _ -> focusDecide g ds' dsOriginal c goal

and focusRight g ds c goal =
    print_endline ("focusRight: " ^ type_to_string goal);
    match goal with 
    | TSendS(t1, t2) -> 
        let y = fresh_channel() in
        let g, da', ds', e1 = focusRight g ds y t1 in
        let g, da'', ds'', e2 = focusRight g ds' c t2 in
        g, da'', ds'', SendS(c, y, e1 , e2)
    | TUnit ->
        g, [], ds, Close(c)
    | TIntChoice(labelsesslist) -> 
        let rec iter_labels labelsesslist =
            match labelsesslist with
            | (l, s)::labelsesslist' ->
                begin 
                    try begin
                        let g, da', ds', e1 = focusRight g ds c s in
                        g, da', ds', ChoiceSelect(c, l, e1)
                    end
                    with Fail _ -> try begin
                        iter_labels labelsesslist'
                    end
                    with Fail m -> raise (Fail m)
                end
            | [] -> raise (Fail "focusLeft: TExtChoice has no valid label-sess option")
        in iter_labels labelsesslist
    | TExponential t ->
        let g', da', ds', e = invertRight g [] ds c goal in
        assert (List.is_empty da' && List.equal (=) ds ds');
        g', da', ds', Exponential(e)
    | _ -> invertRight g [] ds c goal (* goal is not right sync, therefore switch back to inversion phase *)

and focusLeft g ds xFocus tFocus c goal =
    print_endline ("focusLeft: " ^ type_to_string tFocus);
    match tFocus with
    | TRecvS(t1, t2) -> 
        begin try
            let y = fresh_id() in
            let g, da', ds', e2 = focusLeft g ds xFocus t2 c goal in
            let g, da'', ds'', e1 = invertRight g da' ds' y t1 in
            g, da'', ds'', SendS(xFocus, y, e1, e2) 
        with Fail m -> raise (Fail m)
        end
    | TExtChoice(labelsesslist) -> 
        let rec iter_labels labelsesslist =
            match labelsesslist with
            | (l, s)::labelsesslist' ->
                begin 
                    try begin
                        let g, da', ds', e = focusLeft g ds xFocus s c goal in
                        g, da', ds', ChoiceSelect(xFocus, l, e)
                    end
                    with Fail _ -> try begin
                        iter_labels labelsesslist'
                    end
                    with Fail m -> raise (Fail m)
                end
            | [] -> raise (Fail "focusLeft: TExtChoice has no valid label-sess option")
        in iter_labels labelsesslist
    | TAtom _ -> 
        if tFocus = goal then g, [], List.remove_assoc xFocus ds, Var(xFocus)
        else raise (Fail "tFocus != goal")
    | _ -> raise (Fail("focusLeft: somehow foc type is left async: " ^ type_to_string tFocus))

    let synth goal = 
        let g, async', sync', e = invertRight [] [] [] (fresh_channel()) goal in
        if List.is_empty async' && List.is_empty sync' then e
        else raise (Fail("Synthesized expression did not use all linear resources:\n  async: " ^ context_to_string async' ^ "\n  sync: " ^ context_to_string sync' ^ "\n"))

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
