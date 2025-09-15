(* types *)

type id = string

type uOp = Not | Neg

type bOp = And | Or | Eq | Gr | Lt | GrE | LtE | Sum | Sub | Mult | Div

type tyA = (* atomic types (A) *)
    | TInt (* int *)
    | TBool (* bool *)
    | TPolyVar of id (* α *)

type tyK = (* polymorphic kinds (K) *)
    | KBase (* base kind *)
    | KArrow (* arrow kind *)
    
type tyR = (* refinement types (R) *)
    | RTInt of int
    | RTBool of bool
    | RTVar of id (* x *)
    | RTUOp of uOp * tyR
    | RTBOp of bOp * tyR * tyR

and tyF = (* functional types (F) *)
    | TAtomic of tyA (* A *)
    | TRefinement of id * tyA * tyR (* { x:A | R } *)
    | TArrow of tyF * tyF (* F1 -> F2 *)
    | TProcess of (id * tyS) list * tyS (* { c1:S1, ..., cn:Sn |- S } *)
    | TDeclr of id (* variable bound to a previously defined functional type declaration *)
    | TForAll of (id * tyK) list * tyF (* ∀ᾱ. F *)
    | TConstructor of id * tyF list (* T F1 ... Fn *)

and tyS = (* channel/session types (S) *)
    | STSendF of tyF * tyS (* F ∧ S *)
    | STRecvF of tyF * tyS (* F ⊃ S *)
    | STSendS of tyS * tyS (* S1 ⊗ S2 *)
    | STRecvS of tyS * tyS (* S1 -o S2 *)
    | STUnit (* 1 *)
    | STExtChoice of (id * tyS) list (* &{ l1:S1, ..., ln:Sn } *)
    | STIntChoice of (id * tyS) list (* ⊕{ l1:S1, ..., ln:Sn } *)
    | STRec of id * tyS (* 𝜇t. S *)
    | STRecVar of id (* t *)
    | STDeclr of id (* variable bound to a previously defined session type declaration *)
    
(* expressions *)

type expF = (* functional terms (M) *)
    | Int of int
    | Bool of bool
    | UOp of uOp * expF
    | BOp of bOp * expF * expF
    | Var of id (* x *)
    | Let of id * expF * expF (* let x = M1 in M2 *)
    | Lam of id * tyF * expF (* fun x:F -> M *)
    | App of expF * expF (* (M1) M2 *)
    | Ite of expF * expF * expF (* if M1 then M2 else M3 *)
    | Process of id * expP * tyS * (id * tyS) list (* c <- {P :: c : S} <- [c1:S1; ...; cn:Sn] (opaque functional value, P not evaluated) *)
    | LetRec of id * tyF * expF (* let rec x:F = M1 *)
    | Constructor of id * expF list (* C M1 ... Mn *)
    | Match of expF * (id * id list * expF) list (* match M1 with C x̄ -> M2 | ... *)

and expP = (* process terms (P) *)
    | SendF of id * expF * expP (* send c M; P : F ∧ S *)
    | RecvF of id * tyF * id * expP (* x:F <- recv c; P : F ⊃ S *)
    | SendS of id * id * expP * expP (* send c1 c2 P2; P : S1 ⊗ S2 *)
    | RecvS of id * tyS * id * expP (* c1:S <- recv c2; P : S1 -o S2 *)
    | Close of id (* close c : 1 *)
    | Wait of id * expP (* wait c; P *)
    | Fwd of id * id * tyS (* fwd c1 c2 :: c2 : S1 *)
    | Choice of id * (id * expP) list (* case c of li:Pi :: c : &{ l1:S1, ..., ln:Sn } *)
    | ChoiceSelect of id * id * expP (* c.l; P :: c : ⊕{ l1:S1, ..., ln:Sn } *)
    | Spawn of id * expF * id list * expP (* c <- spawn M [c1; ...; cn]; P *)
    