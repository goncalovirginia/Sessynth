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

(* len : ∀a. a -> int -- the return type mentions none, so 'a' is only settled
   by the choice made at the head *)
let len_scheme = scheme ["a"] (TArrow(poly "a", TAtomic TInt))

(* use : ∀a. a -> (a -> int) -> int -- 'a' reaches two argument positions, one of
   them under a binder whose annotation records it *)
let use_scheme =
    scheme ["a"] (TArrow(poly "a", TArrow(TArrow(poly "a", TAtomic TInt), TAtomic TInt)))

(* app : ∀a b. (a -> b) -> a -> b -- 'b' is settled by the goal, 'a' is not *)
let app_scheme =
    scheme ["a"; "b"] (TArrow(TArrow(poly "a", poly "b"), TArrow(poly "a", poly "b")))

let rigid r = TAtomic(TRigidVar r)

(* name, depth budget, Ψ, goal, the solution the search must reach first --
   [None] where the goal must have none at all.

   The budget is what stops a scheme from being applied to itself over and over,
   so it is pinned per case: one that is a step too tight would reject a term for
   running out of room rather than for the reason the case is about. *)
let cases = [
    ("id at int", 7, [("id", id_scheme)], TAtomic TInt, Some (App(Var "id", Int 1)));
    (* one binding answering a second goal is what a scheme buys over an arrow *)
    ("id at bool", 7, [("id", id_scheme)], TAtomic TBool, Some (App(Var "id", Bool true)));
    (* nothing determines 'a', so the head picks the first ground type for it *)
    ("len at int", 9, [("len", len_scheme)], TAtomic TInt, Some (App(Var "len", Int 1)));
    (* the lambda's annotation is the witness: it reads int, not the variable the
       spine started with, because the choice was made before the spine was walked *)
    ("use at int", 12, [("use", use_scheme)], TAtomic TInt,
        Some (App(App(Var "use", Int 1), Lam("_x0", TAtomic TInt, Var "_x0"))));
    ("app at int", 12, [("app", app_scheme)], TAtomic TInt,
        Some (App(App(Var "app", Lam("_x0", TAtomic TInt, Var "_x0")), Int 1)));

    (* offering a scheme rather than using one: 'a' is whatever the caller picked,
       so the argument is the only thing that can supply one *)
    ("goal ∀a. a -> a", 10, [], scheme ["a"] (TArrow(poly "a", poly "a")),
        Some (Lam("_x0", rigid "_ρ0", Var "_x0")));
    (* uninhabited -- no int becomes a type nobody has named yet. flexibly opened
       this answers _x0 -> 1, a function claiming every 'a' that only serves int *)
    ("goal ∀a. int -> a", 10, [], scheme ["a"] (TArrow(TAtomic TInt, poly "a")), None);
    (* a rigid goal is still reachable, just only by passing a value of it through *)
    ("goal ∀a. (int -> a) -> a", 12, [],
        scheme ["a"] (TArrow(TArrow(TAtomic TInt, poly "a"), poly "a")),
        Some (Lam("_x0", TArrow(TAtomic TInt, rigid "_ρ0"), App(Var "_x0", Int 1))));
]

let () =
    set_mode Auto;
    let run (name, depth, p, goal, expected) =
        set_max_depth depth;
        let actual = try Some (synth 1 [] p [] goal) with Fail _ -> None in
        if actual = expected then None
        else
            let show = function
                | Some e -> expF_to_string e 0
                | None -> "no solution"
            in Some (name ^ ": got " ^ show actual ^ ", expected " ^ show expected)
    in
    match List.filter_map run cases with
    | [] -> print_endline "\npolymorphism: all cases passed"
    | failures -> List.iter (fun m -> print_endline ("FAIL " ^ m)) failures; exit 1
