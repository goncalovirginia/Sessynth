exception Fail of string

(* Types and terms *)

type id = string

type ty = 
    | TAtom of id (* string encoding any atom type *)
    | TArrow of ty * ty (* T1 -o T2 *)
    | TMultConjPair of ty * ty (* T1 ⊗ T2 *)
    | TAddConjPair of ty * ty (* T1 & T2 *)
    | TAddDisjCase of ty * ty (* T1 ⊕ T2 *)

type exp = 
    | Let of id * exp * exp (* let x = e1 in e2 *)
    | Var of id (* x *)
    | Lam of id * ty * exp (* x:t -o e *)
    | App of exp  * exp (* (x:t -o e) e *)
    | MultConjPair of exp * exp (* (e1 * e2) : T1 ⊗ T2 *)
    | Let2 of id * id * exp * exp (* let x * y = e1 in e2 *)
    | AddConjPair of exp * exp (* (M1 * M2) : T1 & T2 *)
    | AddConjFst of exp (* fst (M1 * M2) -> M1 *)
    | AddConjSnd of exp (* snd (M1 * M2) -> M2 *)
    | AddDisjInL of exp * ty (* e:T1 then inl_T2 e : T1 ⊕ T2 *)
    | AddDisjInR of ty * exp (* e:T2 then inr_T1 e : T1 ⊕ T2 *)
    | AddDisjCase of exp * id * exp * id * exp (* case e of inl x -> e1, inr y -> e2 *)
    (*
    TODO: process expressions
    | Send of id * exp * exp (* send c e; P *)
    | Recv of id * ty * id * exp (* x:T <- recv c; P *)
    | Close of id (* close c *)
    | Wait of id * exp (* wait c; P *)
    | Fwd of id * id (* fwd d c *)
    *)

(* Auxiliary functions *)

let fresh_id = 
    let unique = ref (-1) in
    fun () -> (incr unique; "x" ^ (string_of_int !unique))
let rec type_to_string t =
    match t with 
    | TAtom a -> a
    | TArrow(t1, t2) -> type_to_string t1 ^ " -o " ^ type_to_string t2
    | TMultConjPair(t1, t2) -> "(" ^ type_to_string t1 ^ " ⊗ " ^ type_to_string t2 ^ ")"
    | TAddConjPair(t1, t2) -> "&{" ^ type_to_string t1 ^ "; " ^ type_to_string t2 ^ "}"
    | TAddDisjCase(t1, t2) -> "⊕{" ^ type_to_string t1 ^ "; " ^ type_to_string t2 ^ "}"

let rec exp_to_string e = 
    match e with 
    | Let(x, e1, e2) -> "let" ^ x ^ " = " ^ exp_to_string e1 ^ " in " ^exp_to_string e2
    | Var x -> x
    | Lam(x, t, e) -> x ^ ":" ^ type_to_string t ^ " -o " ^ exp_to_string e 
    | App(e1, e2) -> "(" ^ exp_to_string e1 ^ ") " ^ exp_to_string e2
    | MultConjPair(e1, e2) -> "(" ^ exp_to_string e1 ^ " ⊗ " ^ exp_to_string e2 ^ ")"
    | Let2(x1, x2, e1, e2) -> "let " ^ x1 ^ ", " ^ x2 ^ " = " ^ exp_to_string e1 ^ " in " ^exp_to_string e2
    | AddConjPair(e1, e2) -> "(" ^ exp_to_string e1 ^ " & " ^ exp_to_string e2 ^ ")"
    | AddConjFst e -> "fst " ^ exp_to_string e
    | AddConjSnd e -> "snd " ^ exp_to_string e
    | AddDisjInL(e, t) -> exp_to_string e ^ ":" ^ type_to_string t ^ " then inl " ^ exp_to_string e ^ ":" ^ type_to_string t ^ "⊕ T2"
    | AddDisjInR(t, e) -> exp_to_string e ^ ":" ^ type_to_string t ^ " then inr " ^ exp_to_string e ^ ":" ^ "T1 ⊕" ^ type_to_string t
    | AddDisjCase(e, x1, e1, x2, e2) -> "case " ^ exp_to_string e ^ "of inl " ^ x1 ^ " -> " ^ exp_to_string e1 ^ ", " ^ "inr " ^ x2 ^ " -> " ^ exp_to_string e2

let rec print_context c = 
    match c with
    | [] -> print_endline "";
    | (x, t)::c' -> print_string(x ^ ":" ^ (type_to_string t) ^ "; ") ; print_context c'

let rec subst e1 x e2 =
    match e1 with 
    | Var y -> if x=y then e2 else e1 
    | App(e, e') -> App (subst e x e2 , subst e' x e2)
    | Lam(y, t, e) -> if x <> y then Lam (y,t,(subst e x e2)) else e1 
    | AddConjPair(e, e') -> AddConjPair (subst e x e2 , subst e' x e2)
    | AddConjFst e -> AddConjFst (subst e x e2)
    | AddConjSnd e -> AddConjSnd (subst e x e2)
    | _ -> raise (Fail("subst pattern matching not defined for " ^ exp_to_string e1))

let is_left_async t = 
    match t with
    | TMultConjPair _ | TAddDisjCase _ -> true
    | _ -> false

let rec append_bindings async sync bindings =
    match bindings with
    | (x, t)::bindings' ->
        if is_left_async t then append_bindings ((x, t)::async) sync bindings'
        else append_bindings async ((x, t)::sync) bindings'
    | [] -> async, sync

(* Focused type-driven synthesizer *)

module Sessynth = struct 

let rec invertRight async sync goal = 
    print_endline ("invertRight: " ^ type_to_string goal);
    print_string "  async: "; print_context async;
    print_string "  sync: "; print_context sync;
    match goal with 
    | TArrow(t1, t2) -> 
        let x = fresh_id() in
        let async', sync' = append_bindings async sync [(x, t1)] in
        Lam(x, t1, invertRight async' sync' t2)
    | TAddConjPair(t1, t2) -> 
        AddConjPair(invertRight async sync t1, invertRight async sync t2)
    | TAtom a -> focusDecide sync sync goal
    | _ -> invertLeft async sync goal

and invertLeft async sync goal =
    print_endline ("invertLeft: " ^ type_to_string goal);
    print_string "  async: "; print_context async;
    print_string "  sync: "; print_context sync;
    match async with
    | (x, xt)::async' ->
        begin match xt with 
        | TMultConjPair(t1, t2) ->
            let x1, x2 = fresh_id(), fresh_id() in
            let async'', sync'' = append_bindings async' sync [(x1, t1); (x2, t2)] in
            Let2(x1, x2, Var(x), invertLeft async'' sync'' goal)
        | TAddDisjCase(t1, t2) ->
            let x1, x2 = fresh_id(), fresh_id() in
            let async1, sync1 = append_bindings async' sync [(x1, t1)] in
            let async2, sync2 = append_bindings async' sync [(x2, t2)] in
            AddDisjCase(Var(x), x1, invertLeft async1 sync1 goal, x2, invertLeft async2 sync2 goal)
        | _ -> raise (Fail("invertLeft: somehow a sync type wound up in async context"))
        end
    | [] -> focusDecide sync sync goal

and focusDecide sync syncOriginal goal = 
    print_endline ("focusDecideR: " ^ type_to_string goal);
    print_string "  sync: "; print_context sync;
    match sync with
    | [] -> raise (Fail "focusDecideR: empty sync context")
    | (x, t)::sync' -> 
        try 
            if is_left_async goal then focusRight syncOriginal goal
            else focusLeft syncOriginal x t goal
        with Fail _ -> focusDecide sync' syncOriginal goal

and focusRight sync goal =
    print_endline ("focusRight: " ^ type_to_string goal);
    match goal with 
    | TMultConjPair(t1, t2) -> 
        MultConjPair(focusRight sync t1, focusRight sync t2)
    | TAddDisjCase(t1, t2) -> 
        begin
            try begin
                let e1 = focusRight sync t1 in
                AddDisjInL(e1, t2)
            end
            with Fail _ -> try begin
                let e2 = focusRight sync t2 in
                AddDisjInR(t1, e2)
            end
            with Fail m -> raise (Fail m)
        end
    | _ -> invertRight [] sync goal

and focusLeft sync id foc goal =
    print_endline ("focusLeft: " ^ type_to_string foc);
    match foc with
    | TArrow(t1, t2) -> 
        begin try
            let e1 = invertRight sync sync t1 in (* . |- e1 : t1 *)
            let x = fresh_id() in
            let e2 = focusLeft sync x t2 goal in (* ... x:t2 |- e2:goal *) 
            subst e2 x (App(Var(id), e1)) (* ... id:t1 -o t2 |- e2[x:=(id, e1)]:goal *)
        with Fail m -> raise (Fail m)
        end
    | TAddConjPair(t1, t2) -> 
            begin 
                try begin
                    let e = focusLeft sync id t1 goal in (* ... y:t1 |- e:goal *)
                    subst e id (AddConjFst(Var(id))) (* ... id:t1&t2 |- e{id=(fst id)}:goal *)
                end
                with Fail _ -> try begin
                    let e = focusLeft sync id t2 goal in
                    subst e id (AddConjSnd(Var(id)))
                end
                with Fail m -> raise (Fail m)
            end
    | TAtom _ -> if foc = goal then Var(id) else raise (Fail "foc != goal")
    | _ -> raise (Fail("focusLeft: somehow foc type is left async: " ^ type_to_string foc))

    let synth goal = invertRight [] [] goal

end;;

(* Running stuff *)

let synthType = 
    (*TArrow(TAddConjPair(TAtom("bool"), TAtom("int")), TAtom("int")) in*)
    TArrow(TMultConjPair(TAtom("int"), TAtom("bool")), TMultConjPair(TAtom("bool"), TAtom("int"))) in
let exp = Sessynth.synth synthType in
print_endline "" ; print_endline (exp_to_string exp)

(* x:(int ⊗ bool) -o let y, z = x in (z ⊗ y) *)
(* Lam("x_0", TMultConjPair(TAtom("int"), TAtom("bool")), Let2("x_1", "x_2", Var("x_0"), MultConjPair(Var("x_2"), Var("x_1")))) *)
