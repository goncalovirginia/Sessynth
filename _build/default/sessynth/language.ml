(* Types and terms *)

type id = string

type uOp = Not | Neg

type bOp = And | Or | Eq | Gr | Lt | GrE | LtE | Sum | Sub | Mult | Div

type tyA = (* atomic types (A) *)
    | TInt
    | TBool
    
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
    | TProcess of tyS list * tyS (* { S1, ..., Sn |- P :: c : S } *)
    
and tyS = (* channel/session types (S) *)
    | STSendF of tyF * tyS (* F ∧ S *)
    | STRecvF of tyF * tyS (* F ⊃ S *)
    | STSendS of tyS * tyS (* S1 ⊗ S2 *)
    | STRecvS of tyS * tyS (* S1 -o S2 *)
    | STUnit (* 1 *)
    | STExtChoice of (id * tyS) list (* &{ l1:S1, ..., ln:Sn } *)
    | STIntChoice of (id * tyS) list (* ⊕{ l1:S1, ..., ln:Sn } *)
    | STRec of id * tyS (* mu t . S *)
    | STRecVar of id (* t *)
    | STDeclr of id * tyS * tyS (* stype x = S1; S2 *)
    
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
    | LetRec of id * expF (* let rec x = M1 *)
    
and expP = (* process terms (P) *)
    | SendF of id * expF * expP (* send c M; P : F ∧ S *)
    | RecvF of id * id * expP (* x:F <- recv c; P : F ⊃ S *)
    | SendS of id * id * expP * expP (* send c1 c2 P2; P : S1 ⊗ S2 *)
    | RecvS of id * id * expP (* x:S <- recv c; P : S1 -o S2 *)
    | Close of id (* close c : 1 *)
    | Wait of id * expP (* wait c; P *)
    | Fwd of id * id * tyS (* fwd c1 c2 :: c2 : S1 *)
    | Choice of id * (id * expP) list (* case c of li:Pi :: c : &{ l1:S1, ..., ln:Sn } *)
    | ChoiceSelect of id * id * expP (* c.l; P :: c : ⊕{ l1:S1, ..., ln:Sn } *)
    | Spawn of id * expF * id list * expP (* c <- spawn M [c1; ...; cn]; P *)
    