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

(* int ^ 1 -- send an int, then close *)
let int_then_close = STSendF(TAtomic TInt, STUnit)

(* (int -> int) ^ 1 -- the payload is where a session type carries a functional
   one, so it is where a variable has to stand for something other than a base *)
let fun_then_close = STSendF(TArrow(TAtomic TInt, TAtomic TInt), STUnit)

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

    (* spawning a process whose protocol is a scheme: the binding has to be opened
       before it can be asked what it offers, and the goal is what settles the 'a'.
       Left unopened the search cannot see it at all and builds the session inline *)
    ("spawn p : ∀a. {a ^ 1}", 14,
        [("p", scheme ["a"] (TProcess([], STSendF(poly "a", STUnit))))],
        TProcess([], int_then_close),
        Some (Process("_c0", Spawn("_c1", Var "p", [], Fwd("_c1", "_c0", int_then_close)),
                      int_then_close, [])));
    (* the same spawn where 'a' has to become a function type. unification refusing
       anything but a base type left p out of the running, and the search built the
       send inline instead *)
    ("spawn p : ∀a. {a ^ 1} at a function payload", 16,
        [("p", scheme ["a"] (TProcess([], STSendF(poly "a", STUnit))))],
        TProcess([], fun_then_close),
        Some (Process("_c0", Spawn("_c1", Var "p", [], Fwd("_c1", "_c0", fun_then_close)),
                      fun_then_close, [])));
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
