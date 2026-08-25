(* Regression tests for the polymorphism rules.

   sessint's own type grammar has no schemes, so a polymorphic binding cannot
   reach Ψ from a .sessint file and the golden harness under sessint/test/rules
   cannot exercise these rules at all. The contexts are built directly instead.

   Each case pins the depth budget the way those goldens do, so the search space
   is small and the solution the search reaches first is deterministic. *)

open Sessynth
open Sessynth.Language

let poly a = TAtomic(TPolyVar a)
let scheme vars t = TForAll(List.map (fun a -> (a, KBase)) vars, t)

(* id : ∀a. a -> a -- the return type mentions every variable, so unifying it
   with the goal grounds the whole spine *)
let id_scheme = scheme ["a"] (TArrow(poly "a", poly "a"))

(* len : ∀a. a -> int -- the return type mentions none, so 'a' is still free
   when the argument is searched *)
let len_scheme = scheme ["a"] (TArrow(poly "a", TAtomic TInt))

(* name, depth budget, Ψ, goal, the solution the search must reach first.

   The budget is what stops a scheme from being applied to itself over and over,
   so it is pinned per case: one that is a step too tight would reject a term for
   running out of room rather than for the reason the case is about. *)
let cases = [
    ("id at int", 7, [("id", id_scheme)], TAtomic TInt, App(Var "id", Int 1));
    (* one binding answering a second goal is what a scheme buys over an arrow *)
    ("id at bool", 7, [("id", id_scheme)], TAtomic TBool, App(Var "id", Bool true));
    (* room to spare for (len) 1: the focus is refused on its type, not its
       depth, so the search falls back on building the goal itself *)
    ("len at int", 9, [("len", len_scheme)], TAtomic TInt, Int 1);
]

let () =
    set_mode Auto;
    let run (name, depth, p, goal, expected) =
        set_max_depth depth;
        match synth 1 [] p [] goal with
        | e when e = expected -> None
        | e -> Some (name ^ ": got " ^ expF_to_string e 0
                     ^ ", expected " ^ expF_to_string expected 0)
        | exception Fail msg -> Some (name ^ ": " ^ msg)
    in
    match List.filter_map run cases with
    | [] -> print_endline "\npolymorphism: all cases passed"
    | failures -> List.iter (fun m -> print_endline ("FAIL " ^ m)) failures; exit 1
