(* Regression tests for the ADT rules.

   sessint has no datatype syntax, so nothing under sessint/test/rules can reach
   these rules; the constructors context is built directly instead.

   Each case pins the depth budget, so the search space is small and the solution
   the search reaches first is deterministic. *)

open Sessynth
open Sessynth.Language

let i = TAtomic TInt
let b = TAtomic TBool

(* the budget a constructor scheme is written with is a placeholder: every
   instantiation overwrites it from the target *)
let list_ k = TConstructor(k, "List", [])
let nil = ("Nil", TForAll([], list_ 0))
let cons = ("Cons", TForAll([], TArrow(i, TArrow(list_ 0, list_ 0))))
let lists = [nil; cons]

(* a wrapper, so the goal's budget has to reach through one constructor to build
   the list inside *)
let wrapped_ k = TConstructor(k, "Wrapped", [])
let wraps = ("Wrap", TForAll([], TArrow(list_ 0, wrapped_ 0))) :: lists

let pair_ k = TConstructor(k, "Pair", [])
let pairs = [("MkPair", TForAll([], TArrow(i, TArrow(b, pair_ 0))))]

(* mutually recursive: neither name ever recurs on itself directly, so only a
   budget that every datatype occurrence spends makes this terminate *)
let tree_ k = TConstructor(k, "Tree", [])
let forest_ k = TConstructor(k, "Forest", [])
let trees = [
    ("Node", TForAll([], TArrow(forest_ 0, tree_ 0)));
    ("Empty", TForAll([], forest_ 0));
    ("More", TForAll([], TArrow(tree_ 0, TArrow(forest_ 0, forest_ 0))));
]

let cn x args = Constructor(x, args)

(** What a case expects. [Rejected] names a fragment the failure message has to
    carry, since a malformed input also leaves the search with nothing to find and
    [NoSol] alone would not tell the two apart. *)
type expected = Sol of expF | NoSol | Rejected of string

(* (name, depth, constructors, Ψ, goal, expected) *)
let cases = [
    (* --- the constructors context is checked at the entry point --- *)
    ("a constructor declared twice is rejected", 8, [nil; nil], [], list_ 1,
        Rejected "declared more than once");
    ("a constructor that is not a scheme is rejected", 8,
        [("Mk", TArrow(i, list_ 0))], [], list_ 1, Rejected "is not a type scheme");
    ("a constructor not ending at a datatype is rejected", 8,
        [("Mk", TForAll([], TArrow(i, i)))], [], list_ 1, Rejected "does not end at a datatype");
    ("an existential constructor is rejected", 8,
        (* ∀a. a -> List, whose result determines no 'a' for the argument search *)
        [("Mk", TForAll(["a"], TArrow(TAtomic(TPolyVar "a"), list_ 0)))], [], list_ 1,
        Rejected "result type does not determine");
    ("a datatype with no constructors is rejected in the goal", 8, [], [], list_ 1,
        Rejected "the datatype List has no constructors, in the goal");
    ("a datatype with no constructors is rejected in psi", 8, [], [("l", list_ 1)], i,
        Rejected "the datatype List has no constructors, in the type of l");
    ("a constructor that fixes its datatype's parameter is rejected", 8,
        (* T int, which a sibling constructor at T bool could not share a match with *)
        [("MkI", TForAll([], TConstructor(0, "T", [i])))], [], TConstructor(1, "T", [i]),
        Rejected "at a type it fixes");
    ("a constructor returning its datatype at an unbound name is rejected", 8,
        [("Mk", TForAll([], TConstructor(0, "T", [TAtomic(TPolyVar "a")])))], [],
        TConstructor(1, "T", [i]), Rejected "which it does not bind");
    ("a constructor repeating a parameter is rejected", 8,
        [("Mk", TForAll(["a"], TConstructor(0, "T", [TAtomic(TPolyVar "a"); TAtomic(TPolyVar "a")])))],
        [], TConstructor(1, "T", [i; i]), Rejected "twice");
    ("constructors disagreeing on a datatype's arity are rejected", 8,
        [("Nought", TForAll([], TConstructor(0, "T", [])));
         ("One", TForAll(["a"], TConstructor(0, "T", [TAtomic(TPolyVar "a")])))],
        [], TConstructor(1, "T", []), Rejected "parameters by one constructor");
    ("a datatype used at the wrong arity is rejected", 8, lists, [], TConstructor(1, "List", [i]),
        Rejected "used with 1 parameters but declared with 0");

    (* --- the budget bounds how deep a value may nest --- *)
    ("nothing builds a datatype whose budget is spent", 8, lists, [], list_ 0, NoSol);
    ("one level of budget builds the nullary constructor", 8, lists, [], list_ 1,
        Sol (cn "Nil" []));
    ("a constructor argument is one level below its result", 10,
        (* the goal's budget has to pay for the wrapper and the list under it *)
        wraps, [], wrapped_ 1, NoSol);
    ("and is buildable once the budget reaches it", 10, wraps, [], wrapped_ 2,
        Sol (cn "Wrap" [cn "Nil" []]));

    (* --- matching --- *)
    ("a spent datatype is inert, so nothing matches on it", 8, lists, [("l", list_ 0)], i,
        Sol (Int 1));
    ("a branch body may use the pattern variables", 10, lists, [("l", list_ 1)], i,
        Sol (Match(Var "l", [("Nil", [], Int 1); ("Cons", ["_x0"; "_x1"], Var "_x0")])));
    ("the recursive argument shrinks, so inversion bottoms out", 14, lists,
        [("l", list_ 2)], i,
        Sol (Match(Var "l", [
            ("Nil", [], Int 1);
            ("Cons", ["_x0"; "_x1"],
                Match(Var "_x1", [
                    ("Nil", [], Var "_x0");
                    ("Cons", ["_x2"; "_x3"], Var "_x2")]))])));
    ("mutual recursion terminates", 14, trees, [("t", tree_ 2)], i,
        Sol (Match(Var "t", [
            ("Node", ["_x0"],
                Match(Var "_x0", [
                    ("Empty", [], Int 1);
                    ("More", ["_x1"; "_x2"], Int 1)]))])));

    (* --- constructor arguments are decided, not kept in right focus --- *)
    ("a constructor argument may come from psi", 10, pairs, [("v", i); ("w", b)], pair_ 1,
        Sol (cn "MkPair" [Var "v"; Var "w"]));

    (* --- left focus on a datatype --- *)
    ("a function returning a datatype answers a datatype goal", 10, lists,
        [("mk", TArrow(i, list_ 1))], list_ 1, Sol (App(Var "mk", Int 1)));
    ("what a call produced is released and case-analysed", 14, lists,
        [("mk", TArrow(i, list_ 1))], i,
        Sol (Let("_x0", App(Var "mk", Int 1),
            Match(Var "_x0", [("Nil", [], Int 1); ("Cons", ["_x1"; "_x2"], Var "_x1")]))));
    ("an inert datatype is not released", 10, lists, [("mk", TArrow(i, list_ 0))], i,
        Sol (Int 1));
]

(* the failure message, so that Rejected can tell one apart from an empty search *)
let contains haystack needle =
    let n = String.length needle and h = String.length haystack in
    let rec at k = k + n <= h && (String.sub haystack k n = needle || at (k + 1)) in
    at 0

let () =
    set_mode Auto;
    let run (name, depth, c, p, goal, expected) =
        set_max_depth depth;
        let actual = try Ok (synth 1 [] p c [] goal) with Fail m -> Error m in
        let describe = function
            | Ok e -> "the solution " ^ expF_to_string e 0
            | Error m -> "the failure " ^ m
        in
        let ok =
            match expected, actual with
            | Sol e, Ok e' -> e = e'
            | NoSol, Error m -> contains m "No valid expression"
            | Rejected frag, Error m -> contains m frag
            | _ -> false
        in
        if ok then None
        else
            let want = match expected with
                | Sol e -> "the solution " ^ expF_to_string e 0
                | NoSol -> "no solution"
                | Rejected frag -> "a failure mentioning " ^ frag
            in Some (name ^ ": got " ^ describe actual ^ ", expected " ^ want)
    in
    match List.filter_map run cases with
    | [] -> print_endline "\nadts: all cases passed"
    | failures -> List.iter (fun m -> print_endline ("FAIL " ^ m)) failures; exit 1
