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
    (*| Send of id * exp * exp (* send c e; P *)
    | Recv of id * ty * id * exp (* x:T <- recv c; P *)
    | Close of id (* close c *)
    | Wait of id * exp (* wait c; P *)
    | Fwd of id * id (* fwd d c *)*)

(* Auxiliary functions *)

let fresh_id = 
    let unique = ref (-1) in
    fun () -> (incr unique; "x_" ^ (string_of_int !unique))
let rec type_to_string t =
    match t with 
    | TAtom a -> a
    | TArrow(t1, t2) -> "(" ^ type_to_string t1 ^ " -o " ^ type_to_string t2 ^ ")"
    | TMultConjPair(t1, t2) -> "(" ^ type_to_string t1 ^ " ⊗ " ^ type_to_string t2 ^ ")"
    | TAddConjPair(t1, t2) -> "(" ^ type_to_string t1 ^ " & " ^ type_to_string t2 ^ ")"
    | TAddDisjCase(t1, t2) -> "(" ^ type_to_string t1 ^ " ⊕ " ^ type_to_string t2 ^ ")"

let rec exp_to_string e = 
    match e with 
    | Let(x, e1, e2) -> "let" ^ x ^ " = " ^ exp_to_string e1 ^ " in " ^exp_to_string e2
    | Var x -> x
    | Lam(x, t, e) -> x ^ ":" ^ type_to_string t ^ " -o " ^ exp_to_string e 
    | App(e1, e2) -> "(" ^ exp_to_string e1 ^ ") " ^ exp_to_string e2
    | MultConjPair(e1, e2) -> "(" ^ exp_to_string e1 ^ " ⊗  " ^ exp_to_string e2 ^ ")"
    | Let2(x1, x2, e1, e2) -> "let" ^ x1 ^ " * " ^ x2 ^ " = " ^ exp_to_string e1 ^ " in " ^exp_to_string e2
    | AddConjPair(e1, e2) -> "(" ^ exp_to_string e1 ^ " & " ^ exp_to_string e2 ^ ")"
    | AddConjFst e -> "fst " ^ exp_to_string e
    | AddConjSnd e -> "snd " ^ exp_to_string e
    | AddDisjInL(e, t) -> exp_to_string e ^ ":" ^ type_to_string t ^ " then inl " ^ exp_to_string e ^ ":" ^ type_to_string t ^ "⊕ T2"
    | AddDisjInR(t, e) -> exp_to_string e ^ ":" ^ type_to_string t ^ " then inr " ^ exp_to_string e ^ ":" ^ "T1 ⊕" ^ type_to_string t
    | AddDisjCase(e, x1, e1, x2, e2) -> "case " ^ exp_to_string e ^ "of inl " ^ x1 ^ " -> " ^ exp_to_string e1 ^ ", " ^ "inr " ^ x2 ^ " -> " ^ exp_to_string e2

let rec subst e1 x e2 =
    match e1 with 
    | Var y -> if x=y then e2 else e1 
    | App(e, e') -> App (subst e x e2 , subst e' x e2)
    | Lam(y, t, e) -> if x <> y then Lam (y,t,(subst e x e2)) else e1 
    | AddConjPair(e, e') -> AddConjPair (subst e x e2 , subst e' x e2)
    | AddConjFst e -> AddConjFst (subst e x e2)
    | AddConjSnd e -> AddConjSnd (subst e x e2)
    | _ -> raise (Fail("subst pattern matching not defined for " ^ exp_to_string e1))

let is_right_async t =
    match t with
    | TArrow _ | TAddConjPair _ -> true
    | _ -> false

let is_left_async t = 
    match t with
    | TMultConjPair _ | TAddDisjCase _ -> true
    | _ -> false

let is_right_sync t = (* TODO *)
    match t with
    | TArrow _ | TAddConjPair _ -> true
    | _ -> false

let is_left_sync t = (* TODO *)
    match t with
    | TArrow _ | TAddConjPair _ -> true
    | _ -> false


let is_async t = is_right_async t || is_right_async t

let rec append_bindings_to_contexts async sync bindings =
    match bindings with
    | (x, t)::bindings' -> 
        if is_async t then append_bindings_to_contexts ((x, t)::async) sync bindings'
        else append_bindings_to_contexts async ((x, t)::sync) bindings'
    | [] -> async, sync

(* Focused type-driven synthesizer *)

module Sessynth = struct 

let rec invertRight async sync t = 
    match t with 
    | TArrow(t1, t2) -> 
        let x = fresh_id() in
        let async', sync' = append_bindings_to_contexts async sync [(x, t1)] in
        Lam(x, t1, invertRight async' sync' t2)
    | TAddConjPair(t1, t2) -> AddConjPair(invertRight async sync t1, invertRight async sync t2)
    | TAtom a -> focusDecide async sync t
    | _ -> invertLeft async sync t

and invertLeft async sync t =
    match async with
    | (x, xt)::async' ->
        begin match xt with 
        | TMultConjPair(t1, t2) ->
            let x1, x2 = fresh_id(), fresh_id() in
            let async'', sync'' = append_bindings_to_contexts async' sync [(x1, t1); (x2, t2)] in
            Let2(x1, x2, Var(x), invertLeft async'' sync'' xt)
        | TAddDisjCase(t1, t2) ->
            let x1, x2 = fresh_id(), fresh_id() in
            let async1, sync1 = append_bindings_to_contexts async' sync [(x1, t1)] in
            let async2, sync2 = append_bindings_to_contexts async' sync [(x2, t2)] in
            AddDisjCase(Var(x), x1, invertLeft async1 sync1 t1, x2, invertLeft async2 sync2 t2)
        | _ -> raise (Fail("invertLeft: somehow a sync type wound up in async context"))
        end
    | [] -> focusDecide sync sync t

and focusDecide sync syncOriginal goal = 
     match sync with
     | [] -> raise (Fail("focusDecide: empty sync context"))
     | (x, t)::sync' -> 
        try 
            if is_right_sync t then focusRight syncOriginal x t goal (* TODO: update is_right_sync and is_left_sync *)
            else if is_left_sync t then focusLeft syncOriginal x t goal
            else raise (Fail "")
        with Fail _ -> focusDecide sync' syncOriginal goal

(* 
Delta, (x:T); [T] |- ??:Atom
----------------------------
Delta |- ??:Atom

pre: Var id : foc
*)

and focusRight sync id foc goal =
    match foc with 
    | TMultConjPair(t1, t2) -> Var("") (* TODO *)
    | TAddDisjCase(t1, t2) -> Var("") (* TODO *)
    | _ -> raise (Fail "foc not right asynchronous")

and focusLeft sync id foc goal =
    match foc with
    | TArrow(t1,t2) -> 
        begin try
            let y = fresh_id () in
            let e2 = focusRight sync y t2 goal in (* ... y:t2 |- e2 : goal *) 
            let e1 = invertRight sync sync t1 in (*  |- e1 : t1 *)
            subst e2 y (App(Var(id), e1)) (* ... id:t1->t2 |- e2[y := (id e1)] : goal  *)
        with Fail m -> raise (Fail m)
        end
    | TAddConjPair(t1, t2) -> 
            begin 
                try begin
                    let e = focusRight sync id t1 goal in (* ... y:t1 |- e : goal *)
                    subst e id (AddConjFst (Var id)) (* ... id:t1&t2  |-     e{id := (Proj1 id)}    : goal *)
                end
                with Fail m -> try begin
                    let e = focusRight sync id t2 goal in
                    subst e id (AddConjSnd (Var id))
                end
                with Fail m -> raise (Fail m)
            end
    | TAtom _ -> if foc = goal then Var id else raise (Fail "foc != goal")
    | _ -> raise (Fail("focusLeft: pattern matching not defined for " ^ type_to_string foc))

end;;

(* Running stuff *)

(* type: TArrow(TMultConjPair(A, B), TMultConjPair(B, A))
expression: Lam("x", TMultConjPair(A, B), Let2("y", "z", Var("x"), MultConjPair(Var("z"), Var("y")))) *)

let targetType = TArrow(TAddConjPair(TAtom("int"), TAtom("bool")), TAtom("int")) in
let exp = Sessynth.invertRight [] [] targetType in
print_endline (exp_to_string exp)
