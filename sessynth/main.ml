open Language;;

(* Running stuff *)

(* Synthesizer *)

let synthType = 
    (*TProcess([], STSendF(TInt, STSendF(TBool, STUnit)))*)
    (*TArrow(TInt, TProcess([], STRec("t", STSendF(TInt, STRecVar("t")))))*)
    (*TArrow(TAtomic(TBool), TArrow(TAtomic(TInt), TProcess([], STRec("t", STSendF(TAtomic(TInt), STSendF(TAtomic(TBool), STRecVar("t")))))))*)
    (*TArrow(TRefinement("x", TInt, RTVar("x")), TRefinement("y", TInt, RTGr(RTVar("y"), RTVar("x"))))*)
    (*TArrow(TRefinement("x", TInt, RTGr(RTVar("x"), RTInt(5))), TArrow(TRefinement("y", TInt, RTGr(RTVar("y"), RTVar("x"))), TRefinement("z", TInt, RTGr(RTVar("z"), RTVar("y")))))*)
    TArrow(TRefinement("x", TInt, RTBool(true)), TArrow(TRefinement("y", TInt, RTBool(true)), TRefinement("z", TInt, RTGr(RTVar("z"), RTSum(RTVar("x"), RTVar("y"))))))
in
let synthExp = Sessynth.synth synthType in
print_endline "\nSynthesized expression:\n" ; print_endline (Sessynth.expF_to_string synthExp);

(* Z3adapter *)

(*print_endline "";
print_endline "Z3adapter test:\n";

let p = 
    (*[("x", TRefinement("x", TInt, RTGr(RTVar("x"), RTInt(5))))]*)
    [("x", TRefinement("x", TBool, RTBool(true)))]
in
let goal = 
    (*TRefinement("y", TInt, RTGr(RTVar("y"), RTVar("x")))*)
    TRefinement("y", TBool, RTAnd(RTVar("y"), RTVar("x")))
in
let id_expF_list = Z3adapter.solve p goal in
List.iter(
    fun (x, e) -> print_endline (x ^ " " ^ Sessynth.expF_to_string e)
) id_expF_list*)

(*
(declare-fun f (Int Int) Int)
(assert (forall ((x Int) (y Int)) (= (f x y) (+ x y))))
(check-sat)
(get-model)
*)
