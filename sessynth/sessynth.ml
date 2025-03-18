exception Fail of string

(* Types and terms *)

type id = string

type ty = 
    | TAtom of id (* string encoding any atom type *)
    | TArrow of ty * ty (* T1 -o T2 *)
    | TMultConjPair of ty * ty (* T1 ⊗ T2 *)
    | TAddConjPair of ty * ty (* T1 & T2 *)
    | TAddDisjPair of ty * ty (* T1 ⊕ T2 *)

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
    | AddDisjPair of exp * id * exp * id * exp (* case e of inl x -> e1, inr y -> e2 *)
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
    | TAddDisjPair(t1, t2) -> "(" ^ type_to_string t1 ^ " ⊕ " ^ type_to_string t2 ^ ")"

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
    | AddDisjPair(e, x1, e1, x2, e2) -> "case " ^ exp_to_string e ^ "of inl " ^ x1 ^ " -> " ^ exp_to_string e1 ^ ", " ^ "inr " ^ x2 ^ " -> " ^ exp_to_string e2

let rec subst e1 x e2 =
    match e1 with 
    | Var y -> if x=y then e2 else e1 
    | App(e, e') -> App (subst e x e2 , subst e' x e2)
    | Lam(y, t, e) -> if x <> y then Lam (y,t,(subst e x e2)) else e1 
    | AddConjPair(e, e') -> AddConjPair (subst e x e2 , subst e' x e2)
    | AddConjFst e -> AddConjFst (subst e x e2)
    | AddConjSnd e -> AddConjSnd (subst e x e2)
    | _ -> raise (Fail("subst pattern matching not defined for " ^ exp_to_string e1))

(* Focused type-driven synthesizer *)

module Sessynth = struct 

let rec inversionRight delta t = 
    match t with 
    | TArrow(t1, t2) -> 
        let x = fresh_id() in
        Lam(x, t1, inversionRight ((x, t1)::delta) t2)
    | TAddConjPair(t1, t2) -> AddConjPair(inversionRight delta t1, inversionRight delta t2)
    | TAtom a -> focus delta delta t    (* Delta |-  ??? : Atom *)
    | _ -> inversionLeft delta delta t

and inversionLeft deltaAsync deltaSync t =
    match deltaAsync with
    | e::xs ->
        begin match e with 
        | x, TMultConjPair(t1, t2) ->
            let x1 = fresh_id() in
            let x2 = fresh_id() in
            let xss = (x1, t1)::(x2, t2)::xs in
            Let2(x1, x2, Var(x), inversionLeft xss deltaSync t)
        | x, TAddDisjPair(t1, t2) -> (* TODO: only 1 t *)
            raise (Fail "")
        | x, sync -> inversionLeft xs ((x, sync)::deltaSync) t
        end
    | [] -> focus deltaSync deltaSync t

(* TArrow(TMultConjPair(A, B), TMultConjPair(B, A))

Lam("x", TMultConjPair(A, B), Let2("y", "z", Var("x"), MultConjPair(Var("z"), Var("y")))) *)

and focus delta c goal = 
     match delta with
     | [] -> raise (Fail("Empty delta context"))
     | (x, t)::xs -> 
        try focus' c x t goal
        with Fail("a") -> focus xs c goal

(* 
Delta, (x:T); [T] |- ??:Atom
----------------------------
Delta |- ??:Atom

pre: Var id : foc
*)

and focus' delta id foc goal =
        match foc with 
        | TArrow(t1,t2) -> 
           begin try
            let y = fresh_id () in
            let e2 = focus' delta y t2 goal in (* ... y:t2 |- e2 : goal *) 
            let e1 = inversionRight delta t1 in (*  |- e1 : t1 *)
            subst e2 y (App (Var id,e1)) (* ... id:t1->t2 |- e2[y := (id e1)] : goal  *)
           with Fail m -> raise (Fail m)
           end
        | TAddConjPair(t1, t2) -> 
            begin 
            try begin
                let e = focus' delta id t1 goal in (* ... y:t1 |- e : goal *)
                subst e id (AddConjFst (Var id)) (* ... id:t1&t2  |-     e{id := (Proj1 id)}    : goal *)
                end
            with Fail m ->
                try begin
                    let e = focus' delta id t2 goal in
                    subst e id (AddConjSnd (Var id))
                    end
                with Fail m -> raise (Fail m)
            end
      | TAtom _ -> if foc = goal then Var id else raise (Fail "foc != goal")
      | _ -> raise (Fail("focus' pattern matching not defined for " ^ type_to_string foc))

end;;

(* Running stuff *)

let delta = [] in
let targetType = TArrow(TAddConjPair(TAtom("int"), TAtom("bool")), TAtom("int")) in
let exp = Sessynth.inversionRight delta targetType in
print_endline (exp_to_string exp)
