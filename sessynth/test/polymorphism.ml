(* Regression tests for the polymorphism rules.

   sessint's own type grammar has no schemes, so a polymorphic binding cannot
   reach Ψ from a .sessint file and the golden harness under sessint/test/rules
   cannot exercise these rules at all. The contexts are built directly instead.

   Each case pins the depth budget the way those goldens do, so the search space
   is small and the solution the search reaches first is deterministic. *)

open Sessynth
open Sessynth.Language

let poly a = TAtomic(TPolyVar a)
let scheme vars t = TForAll(vars, t)

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

(* the same two schemes spelled with a different binder, for the renaming cases *)
let id_scheme_b = scheme ["b"] (TArrow(poly "b", poly "b"))
let len_scheme_b = scheme ["b"] (TArrow(poly "b", TAtomic TInt))

(* a session that sends one value of [t] and closes *)
let sends t = STSendF(t, STUnit)

(* int ^ t ^ 1 -- puts [t] where both sides of a unification can carry a scheme *)
let int_then_scheme t = STSendF(TAtomic TInt, sends t)

(* int ^ 1 -- send an int, then close *)
let int_then_close = STSendF(TAtomic TInt, STUnit)

(* (int -> int) ^ 1 -- the payload is where a session type carries a functional
   one, so it is where a variable has to stand for something other than a base *)
let fun_then_close = STSendF(TArrow(TAtomic TInt, TAtomic TInt), STUnit)

(* two named sessions, one of them the tail of the other *)
let sess_gamma =
    [("Sess", STSendF(TAtomic TInt, STUnit));
     ("Sess2", STSendF(TAtomic TInt, STSendF(TAtomic TInt, STUnit)))]

(* name, depth budget, Γ, Ψ, goal, the solution the search must reach first --
   [None] where the goal must have none at all.

   The budget is what stops a scheme from being applied to itself over and over,
   so it is pinned per case: one that is a step too tight would reject a term for
   running out of room rather than for the reason the case is about. *)
let cases = [
    ("id at int", 7, [], [("id", id_scheme)], TAtomic TInt, Some (App(Var "id", Int 1)));
    (* one binding answering a second goal is what a scheme buys over an arrow *)
    ("id at bool", 7, [], [("id", id_scheme)], TAtomic TBool, Some (App(Var "id", Bool true)));
    (* nothing determines 'a', so the head picks the first ground type for it *)
    ("len at int", 9, [], [("len", len_scheme)], TAtomic TInt, Some (App(Var "len", Int 1)));
    (* the lambda's annotation is the witness: it reads int, not the variable the
       spine started with, because the choice was made before the spine was walked *)
    ("use at int", 12, [], [("use", use_scheme)], TAtomic TInt,
        Some (App(App(Var "use", Int 1), Lam("_x0", TAtomic TInt, Var "_x0"))));
    ("app at int", 12, [], [("app", app_scheme)], TAtomic TInt,
        Some (App(App(Var "app", Lam("_x0", TAtomic TInt, Var "_x0")), Int 1)));

    (* offering a scheme rather than using one: 'a' is whatever the caller picked,
       so the argument is the only thing that can supply one *)
    ("goal ∀a. a -> a", 10, [], [], scheme ["a"] (TArrow(poly "a", poly "a")),
        Some (Lam("_x0", rigid "_ρ0", Var "_x0")));
    (* uninhabited -- no int becomes a type nobody has named yet. flexibly opened
       this answers _x0 -> 1, a function claiming every 'a' that only serves int *)
    ("goal ∀a. int -> a", 10, [], [], scheme ["a"] (TArrow(TAtomic TInt, poly "a")), None);
    (* a rigid goal is still reachable, just only by passing a value of it through *)
    ("goal ∀a. (int -> a) -> a", 12, [], [],
        scheme ["a"] (TArrow(TArrow(TAtomic TInt, poly "a"), poly "a")),
        Some (Lam("_x0", TArrow(TAtomic TInt, rigid "_ρ0"), App(Var "_x0", Int 1))));

    (* spawning a process whose protocol is a scheme: the binding has to be opened
       before it can be asked what it offers, and the goal is what settles the 'a'.
       Left unopened the search cannot see it at all and builds the session inline *)
    ("spawn p : ∀a. {a ^ 1}", 14, [],
        [("p", scheme ["a"] (TProcess([], STSendF(poly "a", STUnit))))],
        TProcess([], int_then_close),
        Some (Process("_c0", Spawn("_c1", Var "p", [], Fwd("_c1", "_c0", int_then_close)),
                      int_then_close, [])));
    (* the same spawn where 'a' has to become a function type. unification refusing
       anything but a base type left p out of the running, and the search built the
       send inline instead *)
    ("spawn p : ∀a. {a ^ 1} at a function payload", 16, [],
        [("p", scheme ["a"] (TProcess([], STSendF(poly "a", STUnit))))],
        TProcess([], fun_then_close),
        Some (Process("_c0", Spawn("_c1", Var "p", [], Fwd("_c1", "_c0", fun_then_close)),
                      fun_then_close, [])));

    (* ∀b. b -> b is ∀a. a -> a, so a process offering one answers a goal asking
       the other. p is arrow-typed on purpose: a binding of exactly the goal's
       type becomes xRecLam, and spawning that is the degenerate self-spawn *)
    ("spawn at an α-equivalent payload", 18, [],
        [("p", TArrow(TAtomic TInt, TProcess([], sends id_scheme_b)))],
        TProcess([], sends id_scheme),
        Some (Process("_c0",
                Spawn("_c1", App(Var "p", Int 1), [], Fwd("_c1", "_c0", sends id_scheme)),
                sends id_scheme, [])));
    (* and renaming a binder is still not the same as changing the body *)
    ("no spawn at a payload that only looks alike", 18, [],
        [("p", TArrow(TAtomic TInt, TProcess([], sends len_scheme_b)))],
        TProcess([], sends id_scheme),
        Some (Process("_c0", SendF("_c0", Lam("_x0", rigid "_ρ0", Var "_x0"), Close "_c0"),
                      sends id_scheme, [])));

    (* two schemes meeting head-on, which only happens where both sides carry one
       in the same payload. unify had no case for them at all and raised *)
    ("spawn where the payloads are both schemes", 18, [],
        [("p", scheme ["a"] (TProcess([], STSendF(poly "a", sends id_scheme_b))))],
        TProcess([], int_then_scheme id_scheme),
        Some (Process("_c0",
                Spawn("_c1", Var "p", [], Fwd("_c1", "_c0", int_then_scheme id_scheme)),
                int_then_scheme id_scheme, [])));
    (* and two schemes still only unify when they are the same type *)
    ("no spawn where the scheme payloads differ", 18, [],
        [("p", scheme ["a"] (TProcess([], STSendF(poly "a", sends id_scheme_b))))],
        TProcess([], int_then_scheme len_scheme),
        Some (Process("_c0",
                SendF("_c0", Int 1,
                    SendF("_c0", Lam("_x0", rigid "_ρ0", Int 1), Close "_c0")),
                int_then_scheme len_scheme, [])));

    (* p names the declaration where the goal spells a step of it out, so the two
       only meet once unification resolves through Γ as equivalence already did *)
    ("spawn where only Γ says the sessions agree", 12, sess_gamma,
        [("p", scheme ["a"] (TProcess([], STSendF(poly "a", STDeclr "Sess"))))],
        TProcess([], STDeclr "Sess2"),
        Some (Process("_c0",
                Spawn("_c1", Var "p", [],
                    Fwd("_c1", "_c0", STSendF(TAtomic TInt, STSendF(TAtomic TInt, STUnit)))),
                STDeclr "Sess2", [])));
    (* validation rather than polymorphism, but it belongs to the same resolving:
       a Γ that leads back to itself denotes no finite type, and both equivalence
       and unification would resolve it until they looped *)
    ("a Γ defined in terms of itself is rejected", 10,
        [("S", STSendF(TAtomic TInt, STDeclr "S"))], [],
        TProcess([("c1", STDeclr "S")], STDeclr "S"), None);
    (* a variable no ∀ binds is nothing the search could determine. right focus
       used to guess a ground type for one, answering a goal it should refuse *)
    ("a type variable no forall binds is rejected", 8, [], [], poly "a", None);
]

let () =
    set_mode Auto;
    let run (name, depth, g, p, goal, expected) =
        set_max_depth depth;
        let actual = try Some (synth 1 g p [] goal) with Fail _ -> None in
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
