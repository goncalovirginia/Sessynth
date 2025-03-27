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
    | SendS of id * id * expP (* send c1 c2; P : S1 ⊗ S2 *)
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
    | TSendChannel(t1, t2) -> "(" ^ type_to_string t1 ^ " ⊗ " ^ type_to_string t2 ^ ")"
    | TRecvChannel(t1, t2) -> type_to_string t1 ^ " -o " ^ type_to_string t2
    | TUnit -> "1"
    | TSendFuncT(t1, t2) -> ""
    | TRecvFuncT(t1, t2) -> ""
    | TExtChoice(xtl) -> "&{" ^ label_type_list_to_string xtl ^ "}"
    | TIntChoice(xtl) -> "⊕{" ^ label_type_list_to_string xtl ^ "}"
    | TSDeclr(x, t) -> ""

let rec exp_to_string e = (* process terms (P) *)
    match e with 
    (* functional terms (M) *)
    | Var x -> x
    | Let(x, e1, e2) -> "let" ^ x ^ " = " ^ exp_to_string e1 ^ " in " ^exp_to_string e2
    | Exponential e -> "!" ^ exp_to_string e
    | LetExponential(x, e1, e2) -> "let !" ^ x ^ " = " ^ exp_to_string e1 ^ " in " ^exp_to_string e2
    | Lam(x, t, e) -> x ^ ":" ^ type_to_string t ^ " -o " ^ exp_to_string e 
    | App(e1, e2) -> "(" ^ exp_to_string e1 ^ ") " ^ exp_to_string e2
    | Process(c, e1, cl) -> ""
    (* process terms (P) *)
    | SendChannel(e1, e2) -> "(" ^ exp_to_string e1 ^ " ⊗ " ^ exp_to_string e2 ^ ")"
    | RecvChannel(e1, e2) -> ""
    | SendFuncTerm(e1, e2) -> ""
    | RecvFuncTerm(e1, e2) -> ""
    | Close(c) -> "1"
    | Wait(c, e) -> ""
    | Fwd(c1, c2) -> ""
    | ExtChoice(c, labelsessl) -> ""
    | ExtChoiceSelect(c, l, labelsessl) -> ""
    | Spawn(c, e1, cl, e2) -> ""

let rec context_to_string c = 
    match c with
    | [] -> "";
    | (x, t)::c' -> x ^ ":" ^ (type_to_string t) ^ "; " ^ context_to_string c'

let rec print_context c = print_endline (context_to_string c)

let rec subst e1 x e2 =
    match e1 with 
    | Var y -> if x=y then e2 else e1 
    | App(e, e') -> App(subst e x e2 , subst e' x e2)
    | Lam(y, t, e) -> if x <> y then Lam(y,t,(subst e x e2)) else e1 
    | Choice(e, e') -> ExtChoice(subst e x e2 , subst e' x e2)
    | ChoiceSelect e -> ExtChoiceSelect (subst e x e2)
    | _ -> raise (Fail("subst pattern matching not defined for " ^ exp_to_string e1))

let is_left_async t = 
    match t with
    | TSendS _ | TIntChoice _ -> true
    | _ -> false

let rec append_bindings async sync bindings =
    match bindings with
    | (x, t)::bindings' ->
        if is_left_async t then append_bindings ((x, t)::async) sync bindings'
        else append_bindings async ((x, t)::sync) bindings'
    | [] -> async, sync

let rec deltas_are_equal inversions prevDelta = 
    let async', sync' = prevDelta in
    match inversions with
    | (_, async'', sync'', _)::inversions' -> List.equal (=) async' async'' && List.equal (=) sync' sync'' && deltas_are_equal inversions' (async'', sync'')
    | [] -> true

let rec get_label_process_list labelsesslist inversions =
    match labelsesslist, inversions with
    | (l, _)::labelsesslist', (_, _, _, expP)::inversions' -> (l, expP)::get_label_process_list labelsesslist' inversions'
    | [], [] | [], _ | _, [] -> []

(* Focused type-driven synthesizer *)

module Sessynth = struct 

let rec invertRight g async sync goal c = 
    print_endline ("invertRight: " ^ type_to_string goal);
    print_string "  async: "; print_context async;
    print_string "  sync: "; print_context sync;
    match goal with 
    | TRecvS(t1, t2) -> 
        let x = fresh_id() in
        let a, s = append_bindings async sync [(x, t1)] in
        let g, async', sync', e = invertRight g a s t2 c in
        assert (List.assoc_opt x async' = None && List.assoc_opt x sync' = None);
        g, async', sync', RecvS(x, c, e)
    | TExtChoice(labelsesslist) ->
        let inversions = List.map (fun (l, s) -> invertRight g async sync s c) labelsesslist in
        let g, a1, s1, _ = List.hd inversions in
        assert (deltas_are_equal (List.tl inversions) (a1, s1));
        let labelprocesslist = get_label_process_list labelsesslist inversions in
        g, a1, s1, Choice(c, labelprocesslist)
    | _ -> invertLeft g async sync goal

and invertLeft g async sync goal =
    print_endline ("invertLeft: " ^ type_to_string goal);
    print_string "  async: "; print_context async;
    print_string "  sync: "; print_context sync;
    match async with
    | (x, t)::async' ->
        begin match t with 
        | TSendChannel(t1, t2) ->
            let x1, x2 = fresh_id(), fresh_id() in
            let a, s = append_bindings async' sync [(x1, t1); (x2, t2)] in
            let g, async', sync', e = invertLeft g a s goal in
            assert (List.assoc_opt x1 async' = None && List.assoc_opt x1 sync' = None && List.assoc_opt x2 async' = None && List.assoc_opt x2 sync' = None);
            g, async', sync', Let2(x1, x2, Var(x), e)
        | TUnit -> 
            invertLeft g async' sync goal
        | TAddDisjCase(t1, t2) ->
            let x1, x2 = fresh_id(), fresh_id() in
            let a1, s1 = append_bindings async' sync [(x1, t1)] in
            let a2, s2 = append_bindings async' sync [(x2, t2)] in
            let g, async', sync', e1 = invertLeft g a1 s1 goal in
            let g, async'', sync'', e2 = invertLeft g a2 s2 goal in
            assert (List.assoc_opt x1 async' = None && List.assoc_opt x1 sync' = None && List.assoc_opt x2 async'' = None && List.assoc_opt x2 sync'' = None && List.equal (=) async' async'' && List.equal (=) sync' sync'');
            g, async'', sync'', AddDisjCase(Var(x), x1, e1, x2, e2)
        | TExponential t ->
            let x1 = fresh_id() in
            let g', async', sync', e = invertLeft ((x1, t)::g) async' sync t in
            g', async', sync', LetExponential(x1, Var(x), e)
        | _ -> raise (Fail("invertLeft: somehow a sync type wound up in async context"))
        end
    | [] -> focusDecide g sync sync goal

and focusDecide g sync syncOriginal goal = 
    print_endline ("focusDecide: " ^ type_to_string goal);
    print_string "  sync: "; print_context sync;
    match sync with
    | [] -> raise (Fail "focusDecide: empty sync context")
    | (xFocus, tFocus)::sync' -> 
        try 
            if is_left_async goal then focusRight g syncOriginal goal
            else focusLeft g syncOriginal xFocus tFocus goal
        with Fail _ -> focusDecide g sync' syncOriginal goal

and focusRight g sync goal =
    print_endline ("focusRight: " ^ type_to_string goal);
    match goal with 
    | TSendChannel(t1, t2) -> 
        let g, async', sync', e1 = focusRight g sync t1 in
        let g, async'', sync'', e2 = focusRight g sync' t2 in
        g, async'', sync'', SendS(e1, e2)
    | TUnit ->
        g, [], sync, Close
    | TAddDisjCase(t1, t2) -> 
        begin
            try begin
                let g, async', sync', e1 = focusRight g sync t1 in
                g, async', sync', IntChoice(e1, t2)
            end
            with Fail _ -> try begin
                let g, async', sync', e2 = focusRight g sync t2 in
                g, async', sync', AddDisjInR(t1, e2)
            end
            with Fail m -> raise (Fail m)
        end
    | TExponential t ->
        let g', async', sync', e = invertRight g [] sync goal in
        assert (List.is_empty async' && List.equal (=) sync sync');
        g', async', sync', Exponential(e)
    | _ -> invertRight g [] sync goal (* goal is not right sync, therefore switch back to inversion phase *)

and focusLeft g sync xFocus tFocus goal =
    print_endline ("focusLeft: " ^ type_to_string tFocus);
    match tFocus with
    | TRecvChannel(t1, t2) -> 
        begin try
            let y = fresh_id() in
            let g, async', sync', e2 = focusLeft g sync y t2 goal in (* ... x:t2 |- e2:goal *) 
            let g, async'', sync'', e1 = invertRight g async' sync' t1 in (* . |- e1 : t1 *)
            g, async'', sync'', subst e2 y (App(Var(xFocus), e1)) (* ... id:t1 -o t2 |- e2[x:=(id, e1)]:goal *)
        with Fail m -> raise (Fail m)
        end
    | TAddConjPair(t1, t2) -> 
            begin 
                try begin
                    let g, async', sync', e = focusLeft g sync xFocus t1 goal in (* ... y:t1 |- e:goal *)
                    g, async', sync', subst e xFocus (ExtChoiceSelect(Var(xFocus))) (* ... id:t1&t2 |- e{id=(fst id)}:goal *)
                end
                with Fail _ -> try begin
                    let g, async', sync', e = focusLeft g sync xFocus t2 goal in
                    g, async', sync', subst e xFocus (AddConjSnd(Var(xFocus)))
                end
                with Fail m -> raise (Fail m)
            end
    | TAtom _ -> 
        if tFocus = goal then g, [], List.remove_assoc xFocus sync, Var(xFocus)
        else raise (Fail "tFocus != goal")
    | _ -> raise (Fail("focusLeft: somehow foc type is left async: " ^ type_to_string tFocus))

    let synth goal = 
        let g, async', sync', e = invertRight [] [] [] goal in
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
