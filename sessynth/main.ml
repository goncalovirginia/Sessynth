open Sessynth.Language;;

(* Running stuff *)

let synthType = 
    (*TProcess([], STSendF(TInt, STSendF(TBool, STUnit)))*)
    (*TArrow(TInt, TProcess([], STRec("t", STSendF(TInt, STRecVar("t")))))*)
    (*TArrow(TAtomic(TBool), TArrow(TAtomic(TInt), TProcess([], STRec("t", STSendF(TAtomic(TInt), STSendF(TAtomic(TBool), STRecVar("t")))))))*)
    (*TArrow(TRefinement("x", TInt, RTVar("x")), TRefinement("y", TInt, RTGr(RTVar("y"), RTVar("x"))))*)
    (*TArrow(TRefinement("x", TInt, RTGr(RTVar("x"), RTInt(0))), TArrow(TRefinement("y", TInt, RTGr(RTVar("y"), RTInt(0))), TRefinement("z", TInt, RTGr(RTVar("z"), RTMult(RTVar("x"), RTVar("y"))))))*)
    (*TArrow(TRefinement("x", TInt, RTBool(true)), TArrow(TRefinement("y", TInt, RTBool(true)), TRefinement("z", TInt, RTGr(RTVar("z"), RTSum(RTVar("x"), RTVar("y"))))))*)
    (*TArrow(TRefinement("x", TInt, RTGr(RTVar("x"), RTInt(0))), TRefinement("z", TInt, RTOr(RTGr(RTVar("z"), RTVar("x")), RTGr(RTVar("z"), RTInt(0)))))*)
    TArrow(TRefinement("x", TInt, RTBool(true)), TArrow(TRefinement("y", TInt, RTBool(true)), TRefinement("z", TInt, RTBOp(And, RTBOp(GrE, RTVar("z"), RTBOp(Mult, RTVar("x"), RTVar("y"))), RTBOp(GrE, RTVar("z"), RTInt(0))))))
in
let exp = Sessynth.synth 3 [] [] synthType in
print_endline "\nSynthesized expressions:\n" ; 
print_endline (Sessynth.expF_to_string exp);

