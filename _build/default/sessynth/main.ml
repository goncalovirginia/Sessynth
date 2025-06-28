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
    TArrow(TRefinement("x", TInt, RTBool(true)), TArrow(TRefinement("y", TInt, RTBool(true)), TRefinement("z", TInt, RTAnd(RTGrE(RTVar("z"), RTMult(RTVar("x"), RTVar("y"))), RTGrE(RTVar("z"), RTInt(0))))))
in
let synthExp = Sessynth.synth [] synthType in
print_endline "\nSynthesized expression:\n" ; print_endline (Sessynth.expF_to_string synthExp);

