exception Fail of string

(* Types and terms *)

type id = string

type ty = 
    | TAtom of id (* id encoding any atom type *)
    | TArrow of ty * ty (* T1 -o T2 *)
    | TAltConjPair of ty * ty (* T1 & T2 *)

type exp = 
    (* linear variable *)
    | LVar of id (* x *)
    (* linear function *)
    | Lam of id * ty * exp (* x:t -o e *)
    | App of exp  * exp (* (x:t -o e) e *)
    (* A & B *)
    | AltConjPair of exp * exp (* (M1 * M2) : T1 & T2 *)
    | AltConjFst of exp (* fst (M1 * M2) -> M1 *)
    | AltConjSnd of exp (* snd (M1 * M2) -> M2 *)

(* Auxiliary functions *)

let fresh_id = 
    let unique = ref (-1) in
    fun () -> (incr unique ; "x_"^(string_of_int !unique))
let rec pp_type t =
    match t with 
    | TAtom a -> a
    | TArrow(t1, t2) -> "("^pp_type t1 ^ " -> " ^ pp_type t2 ^ ")"
    | TAltConjPair(t1, t2) -> "("^pp_type t1 ^ " & " ^ pp_type t2^")"

let rec pp_exp e = 
   match e with 
   | LVar x -> x
   | Lam(x, t, e) -> x ^ ":" ^ pp_type t ^ " -o " ^ pp_exp e 
   | App(e1, e2) -> pp_exp e1 ^ " " ^ pp_exp e2
   | AltConjPair(e1,e2) -> pp_exp e1 ^ " & " ^ pp_exp e2
   | AltConjFst e -> "fst " ^ pp_exp e
   | AltConjSnd e -> "snd " ^ pp_exp e

(* Focused type-driven synthesizer *)

module Sessynth = struct 

let rec subst e1 x e2 =
    match e1 with 
    | LVar y -> if x=y then e2 else e1 
    | App(e, e') -> App (subst e x e2 , subst e' x e2)
    | Lam(y, t, e) -> if x <> y then Lam (y,t,(subst e x e2)) else e1 
    | AltConjPair(e, e')-> AltConjPair (subst e x e2 , subst e' x e2)
    | AltConjFst e -> AltConjFst (subst e x e2)
    | AltConjSnd e -> AltConjSnd (subst e x e2)

let rec inversion delta t = 
    match t with 
    | TArrow(t1, t2) -> 
        let x = fresh_id () in
        Lam(x, t1, inversion ((x, t1)::delta) t2)
    | TAltConjPair(t1, t2) -> AltConjPair(inversion delta t1, inversion delta t2)
    | TAtom a -> focus delta delta t    (* Delta |-  ??? : Atom *)

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
            let e1 = inversion delta t1 in   (*  |- e1 : t1 *)
                subst e2 y (App (LVar id,e1))     
                (* ... id:t1->t2 |-   e2[y :=  (id e1) ]   : goal  *)
           with Fail m -> raise (Fail m)
           end
      | TAltConjPair(t1, t2) -> 
            begin 
            try begin
                let e = focus' delta id t1 goal in (* ... y:t1 |- e : goal *)
                subst e id (AltConjFst (LVar id)) (* ... id:t1&t2  |-     e{id := (Proj1 id)}    : goal *)
                end
            with Fail m ->
                try begin
                    let e = focus' delta id t2 goal in
                    subst e id (AltConjSnd (LVar id))
                    end
                with Fail m -> raise (Fail m)
            end
      | TAtom _ -> if foc = goal then LVar id else raise (Fail "foc != goal")

end;;

(* Running stuff *)

let delta = [] in
let targetType = TArrow(TAltConjPair(TAtom("int"), TAtom("bool")), TAtom("int")) in
let exp = Sessynth.inversion delta targetType in
print_endline (pp_exp exp)
