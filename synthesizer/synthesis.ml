
type id = string

let fresh_id = 
    let unique = ref (-1) in
    fun () -> (incr unique ; "x_"^(string_of_int !unique))



type ty = Atom of id 
        | Arrow of ty * ty
        | PairT of ty * ty 

type tm = Var of id 
        | App of tm  * tm 
        | Lam of id * ty * tm
        | Pair of tm * tm
        | Proj1 of tm 
        | Proj2 of tm



let rec pp_ty t =
    match t with 
    | Atom a -> a
    | Arrow (t1,t2) -> "("^pp_ty t1 ^ " -> " ^ pp_ty t2 ^ ")"
    | PairT (t1,t2) -> "("^pp_ty t1 ^ " * " ^ pp_ty t2^")"

let rec pp e = 
   match e with 
   | Var x -> x
   | App (e1,e2) -> pp e1 ^ " " ^ pp e2
   | Pair (e1,e2) -> "< " ^ pp e1 ^ " , " ^ pp e2 ^ " >"
   | Lam (x,t,e) -> "\\" ^ x ^ " : " ^ pp_ty t ^ " . " ^ pp e 
   | Proj1 e -> "fst " ^ pp e
   | Proj2 e -> "snd " ^ pp e



let rec subst e1 x e2 =
  match e1 with 
  | Var y -> if x=y then e2 else e1 
  | App (e,e') -> App (subst e x e2 , subst e' x e2)
  | Lam (y,t,e) -> if x <> y then Lam (y,t,(subst e x e2)) else e1 
  | Pair (e,e')-> Pair (subst e x e2 , subst e' x e2)
  | Proj1 e -> Proj1 (subst e x e2)
  | Proj2 e -> Proj2 (subst e x e2)

(* Focusing-based synthesis *)
exception Fail 


(* Focusing using  plain lambda-terms, for pedagogical purposes *)
module Plain = struct 

let rec inversion ctxt t = 
    match t with 
    | Arrow (t1,t2) -> 
        let x = fresh_id () in
         Lam (x,t1,(inversion ((x,t1)::ctxt) t2))
    | PairT (t1,t2) ->
        Pair (inversion ctxt t1, inversion ctxt t2)
    | Atom a -> focus ctxt ctxt t    (* Ctxt |-  ??? : Atom *)
and focus ctxt c goal = 
     match ctxt with
     | [] -> raise Fail 
     | (x,t)::xs -> try focus' c x t goal
                    with Fail -> focus xs c goal

(* 

Ctxt ; [T] |- ?? : Atom     (x:T) in Ctxt
----------------------------
Ctxt |- ?? :Atom

*)

(* pre: Var id : foc *)
(* 


*)
and focus' ctxt id foc goal =
      match foc with 
      | Arrow (t1,t2) -> 
           begin try
            let y = fresh_id () in
            let e2 = focus' ctxt y t2 goal in (* ... y:t2 |- e2 : goal *) 
            let e1 = inversion ctxt t1 in   (*  |- e1 : t1 *)
                subst e2 y (App (Var id,e1))     
                (* ... id:t1->t2 |-   e2[y :=  (id e1) ]   : goal  *)
           with Fail -> raise Fail
           end
      | PairT (t1,t2) -> 
            begin 
            try begin
                let e = focus' ctxt id t1 goal in  
                     (* ... y:t1 |- e : goal *)
                subst e id (Proj1 (Var id))           
                end    (* ... id:t1&t2  |-     e{id := (Proj1 id)}    : goal *)
            with Fail ->
                try begin
                    let e = focus' ctxt id t2 goal in
                    subst e id (Proj2 (Var id))
                    end
                with Fail -> raise Fail
            end
      | Atom _ -> if foc = goal then Var id else raise Fail

let synthesize = inversion []
end

(* Now lets throw more proof theory at the problem... *)
(* Syntax for focused terms for optimization *)
type term = L of id * ty * term (* Lambda *)
          | P of term * term (* Pair *)
          | Sp of id * spine (* id:t1*(t2*(t3*t4))    id . (Snd;Snd;Snd)   *)
and 
    spine = A of term * spine   
          | Fst of spine 
          | Snd of spine 
          | Success

let rec inversion ctxt t = 
    match t with 
    | Arrow (t1,t2) -> 
        let x = fresh_id () in
         L (x,t1,(inversion ((x,t1)::ctxt) t2))
    | PairT (t1,t2) ->
        P (inversion ctxt t1, inversion ctxt t2)
    | Atom a -> focus ctxt ctxt t 
and focus ctxt c goal = 
     match ctxt with
     | [] -> raise Fail 
     | (x,t)::xs -> try Sp (x,focus' c t goal)
                    with Fail -> focus xs c goal
and focus' ctxt foc goal =
      match foc with 
      | Arrow (t1,t2) -> 
           begin try
            let e2 = focus' ctxt t2 goal in 
            let e1 = inversion ctxt t1 in 
                A (e1,e2)
           with Fail -> raise Fail
           end
      | PairT (t1,t2) -> 
            begin 
            try begin
                Fst (focus' ctxt t1 goal)
                    
                end
            with Fail ->
                try begin
                    Snd (focus' ctxt t2 goal)
                    end
                with Fail -> raise Fail
            end
      | Atom _ -> if foc = goal then Success else raise Fail

   


(* From spines to plain lambda terms *)

let rec defoc_term e =
  match e with 
  | L (x,t,e) -> Lam (x,t,defoc_term e)
  | P (e1,e2) -> Pair (defoc_term e1, defoc_term e2)
  | Sp (x,sp) -> defoc_spine sp (Var x) 


(*    id . (Snd;Snd;Snd;Success)   ~~~~ Snd(Snd(Snd id)) *)
and defoc_spine sp acc = 
  match sp with 
   | Success -> acc 
   | A(tm,sp) -> defoc_spine sp (App (acc,defoc_term tm))
   | Fst sp -> defoc_spine sp (Proj1 acc)
   | Snd sp -> defoc_spine sp (Proj2 acc)

let synthesize t = defoc_term (inversion [] t)
   
(* (a -> (b,c)) -> a -> (c,b) 
   
  \f: a -> (b,c) . \x:a . < snd (f x) , fst (f x) >


  x:a , f : a -> (b,c)  |-   ??? : c
*)   
let t1 = Arrow(Arrow (Atom "a",PairT(Atom "b", Atom "c")),Arrow(Atom "a",PairT (Atom "c",Atom "b")))
let t2 = Arrow (Arrow (Atom "a",Atom "b"),Arrow(Atom "a",PairT (Atom "b",Atom "b")))
let t3 = Arrow ( Arrow( Atom "a",Atom "b"),
                 Arrow( Arrow( PairT(Atom "b",Atom "b") , Atom "c"),
                 Arrow ( Atom "a" , Atom "c")))
            




(*   RowAbs R . fun r:R -> { a:123 , ..r }   : forallR R . r:R -> { a:Int , R } 
     RowAbs R : {\a}. NewTable<...>{a:Int,R} : forallR R : {\a}. { a:Int , R }


R ; |- M : T
-------------------------------------
  ;;  |-   RowAbs R. M : ForallR R.T


M : ForallR R:{c}.T      Row |= c
---------------------------------
  |-  M[Row]  :  T[R := Row]



c:ForallR R.Table[a:Int,R] , d:forallR R'.Table[a:Int,b:Int] -> Table[a:Int,c:Int] |- ??? : Table[a:Int,c:Int]

d[R1](d[R2](new Table{}))    


 addColumn:Forall T.ForallR R.ForallL lab . Table[R] -> T -> a:Label -> Table{a:T,R} |-       : Table{a:Int,b:Int}

SkolemVarT
SkolemRow


tab:Table[SkolemRow] 
x:SkolemVarT
l:SkolemLabel
addColumn tab x l : Table{a:T,R}       Table{a:Int,b:Int}



c:Table[a:Int,SkolemRow]  |- : Table[a:Int,b:Int] 

SkolemRow = {b:Int}

c[{b:Int}]





*)




module QuantSynthesis = 
struct
    let fresh_tvar =
        let unique = ref (-1) in
        fun () -> (incr unique ;"t_"^(string_of_int !unique))

    type ty = (*Atom of id *)
        | Arrow of ty * ty
        | Forall of id * ty 
        | TVar of id  
        
    let rec tsubst t1 x t2 =
        match t2 with
        | Arrow (s,t) -> Arrow (tsubst t1 x s, tsubst t1 x t)
        | TVar y -> if x=y then t1 else t2 
        | Forall (y,t) -> if x<>y then Forall (y ,tsubst t1 x t) else t2

    type tm = Var of id 
        | App of tm * tm 
        | Lam of id * ty * tm
        | TLam of id * tm 
        | TApp of tm * ty

    let rec gen_instances tctxt depth = assert false
    
    let rec inversion tctxt ctxt t = 
        match t with  
        | Arrow (t1,t2) -> 
            let x = fresh_id () in
            Lam (x,t1,(inversion tctxt ((x,t1)::ctxt) t2))
        | Forall (x,t) -> 
            TLam (x,inversion (x::tctxt) ctxt t )
        | TVar x -> if List.mem x tctxt then focus tctxt ctxt ctxt t else raise Fail

    and  
        focus tctxt ctxt c goal =
            match ctxt with  
             | [] -> raise Fail 
             | (x,t)::xs -> try focus' tctxt c x t goal
                    with Fail -> focus tctxt xs c goal
    and 
        focus' tctxt ctxt id foc goal =
            match foc with  
            | Arrow (t1,t2) -> 
                begin try
                    let e2 = focus' tctxt ctxt id t2 goal in 
                    let e1 = inversion tctxt ctxt t1 in 
                        App (e1,e2) (*TODO: Fix *) 
                with Fail -> raise Fail
                end
            | Forall (x,t) -> 
                    let rec instantiate tc var ty = 
                        match tc with 
                        | [] -> raise Fail 
                        | x::xs -> let  t' = tsubst (TVar x) var ty in
                                      begin  
                                        try 
                                            focus' tctxt ctxt id t' goal 
                                        with Fail -> instantiate xs var ty
                                      end  
                    in 
                        instantiate tctxt x t
                (* 
                    let skolemVar = fresh_tvar() in 
                      let (tm, constraints) = focus' (skolemVar::tctxt) ctxt id t goal in 
                        let subst = unify constraints in
                            apply subst tm
                *)

            | TVar _ -> if foc = goal then Var id else raise Fail

    let synthesize = inversion [] []
end



(**
  Y;     x:Forall X. X -> Y  , a:A    |-  ???? :   Y

            TApp x A   

        SkolemZ->Y      Y bate certo com Y OK

            App x a

        inversion SkolemZ     a    (SkolemZ = A)

        SkolemZ     (A,X)    X |-> A

        ------------------------
        Foc = Forall X. X -> Y  
        Goal = SkolemZ

        SkolemW

        SkolemW -> Y

        Foc = Y
        Goal = SkolemZ     [NOPE]

        ------------
        Foc = a:A
        Goal = SkolemZ









        ---------------------


        Goal: SkolemZ

        Foco: a:A

        A =?= SkolemZ


 *)



module HindleyMilner =
struct 

    (* Type syntax *)

    (* Monomorphic types *)
    type tag = Skolem | Universal
    type tvar = TV of (id * tag)

    type ty = 
        TVar of tvar 
    |   TCon of string 
    |   TArr of ty * ty 

    let skolem_var x = TVar (TV (x,Skolem))
    let un_var x = TVar (TV (x,Universal))

    let isSkolem = function 
        TVar (TV (_ , Skolem)) -> true 
        | _ -> false 
    
    let isUniversal = function
        TVar (TV (_ , Universal)) -> true 
        | _ -> false 


    let typeInt = TCon "Int"
    let typeBool = TCon "Bool"

    let pp_var = function
        TV(x,Skolem) -> x^"#Skolem"
     |  TV(x,Universal) -> x^"#Univ"

    let rec pp_ty t =
        match t with 
        | TCon a -> a
        | TArr (t1,t2) -> "("^pp_ty t1 ^ " -> " ^ pp_ty t2 ^ ")"
        | TVar x -> pp_var x
        

    
    (* Polymorphic types or type schemes *)

    type scheme = Forall of (id list) * ty (* Forall X,Y,Z . T *)

    let rec pp_scheme s = function 
        Forall (xs,t) -> "Forall " ^ String.concat "," xs ^ " . (" ^ pp_ty t ^ ")"


        (* Program syntax *)

    type id = string 

    type lit = LInt of int | LBool of bool 
    
    type binop = Add | Sub | Mul | Eql

    type expr = 
        Var of id
    |   App of expr * expr
    |   TLam of id list * expr 
    |   TApp of expr * ty list
    |   Lam of id * expr 
    |   Let of (id*expr*expr)
    |   Lit of lit 
    |   If of expr * expr * expr 
    |   Fix of expr 
    |   Op of binop * expr * expr 





    type decl = (string * expr)

    type program = 
        Prog of decl list * expr

    let factorial = Fix (Lam ("fact", (Lam ("n",(If (Op(Eql,Var "n",Lit (LInt 0)),
                                        Lit (LInt 1),
                                        Op(Mul,Var "n",App (Var "fact",Op(Sub,Var "n",Lit (LInt 1))))
                                        ))))))

    (** Type utilities *)

    module Env = Map.Make(String)

    

    type type_env = scheme Env.t

    let fresh_tvar =
        let unique = ref (-1) in
        fun () -> (incr unique ;"t_"^(string_of_int !unique))

    (* Free type variables *)
    module Set = Set.Make(String)

    let rec ftv_ty ty = 
        match ty with 
        | TVar (TV (x,_)) -> Set.singleton x
        | TCon _ -> Set.empty
        | TArr (t1,t2) -> Set.union (ftv_ty t1) (ftv_ty t2)
    
    let ftv_scheme s =
        match s with
         Forall (l,t) -> 
            Set.diff (ftv_ty t) (Set.of_list l)

    let ftv_tenv tenv = Env.fold (fun key sch acc -> Set.union (ftv_scheme sch) acc ) tenv Set.empty




    (* Type Substitutions *)
    type ty_subst = ty Env.t

    let rec apply_ty s ty = 
        match ty with 
          TVar (TV(x,_)) -> 
            begin 
                match Env.find_opt x s with 
                | None -> ty 
                | Some t -> t
            end        
        | TCon _ -> ty 
        | TArr (t1,t2) -> TArr (apply_ty s t1, apply_ty s t2)

    let apply_scheme s ty = 
        match ty with 
          Forall (l,t) -> 
            let s' = List.fold_left (fun acc x -> Env.remove x acc) s l in
            Forall (l, apply_ty s' t)

    let apply_tenv s tenv = Env.map (apply_scheme s) tenv

       
    (* Term Substitutions *)
    type tm_subst = tm Env.t 
    let rec apply_tm s (tm:expr) = 
        match tm with
        | Var x -> begin 
                    match Env.find_opt x s with 
                    | None -> tm 
                    | Some e -> e
                end    
        | App (e1, e2) -> App (apply_tm s e1, apply_tm s e2)
        | Lam (x,e) -> Lam (x, apply_tm (Env.remove x s) e)
        | Let (x,e1,e2) -> Let (x,apply_tm s e1, apply_tm (Env.remove x s) e2) 
        | Lit _ -> tm
        | If (e1, e2,e3) ->  If (apply_tm s e1, apply_tm s e2, apply_tm s e3)
        | Fix e -> Fix (apply_tm s e)
        | Op (op, e1, e2) -> Op(op,apply_tm s e1, apply_tm s e2)
        | TLam (x,e) -> TLam (x,apply_tm (List.fold_left (fun acc x -> Env.remove x acc) s x) e) (* safe to not remove perhaps *)
        | TApp (e1,t) -> TApp (apply_tm s e1,t)

    let rec apply_tytm s (tm:expr) = 
        match tm with
        | Var _ -> tm 
        | App (e1, e2) -> App (apply_tytm s e1, apply_tytm s e2)
        | TLam (x,e) -> TLam (x, apply_tytm (List.fold_left (fun acc x -> Env.remove x acc) s x) e)
        | TApp (e, lt) -> TApp (apply_tytm s e, List.map (apply_ty s) lt)
        | Lam (x,e) -> Lam (x, apply_tytm (Env.remove x s) e)
        | Let (x,e1,e2) -> Let (x,apply_tytm s e1, apply_tytm (Env.remove x s) e2) 
        | Lit _ -> tm
        | If (e1, e2,e3) ->  If (apply_tytm s e1, apply_tytm s e2, apply_tytm s e3)
        | Fix e -> Fix (apply_tytm s e)
        | Op (op, e1, e2) -> Op(op,apply_tytm s e1, apply_tytm s e2)

    let emptySubst = Env.empty

    (* Compose substitutions in a left-biased way, that is, 
     * compose s1 s2 will apply s1 to types in s2 as well as merge the substitutions. 
     *)
    let compose s1 s2 = 
        let comp' key b1 b2 = 
            match (b1,b2) with 
            | None , None -> None 
            | None , Some t -> Some (apply_ty s1 t) 
            | Some t , None -> Some t 
            | Some t1 , Some t2 -> Some t1
        in
        Env.merge comp' s1 s2

        

  
    (* Unification *)

  

    let occursCheck x t = Set.mem x (ftv_ty t)

    exception UnificationFailure of (ty * ty)
    exception InfiniteType of (id*ty) 
    exception Skolem of id*id

    let bind x t = 
        match t with 
        | TVar (TV (y,Universal) ) -> if x=y then emptySubst else Env.singleton x t
        | TVar (TV (y,Skolem) ) -> if x=y then emptySubst else Env.singleton y (TVar (TV (x,Universal)))
        | _ -> if occursCheck x t then 
                    raise @@ InfiniteType (x,t)
               else 
                    Env.singleton x t
            

    let rec unify t1 t2 = 
    (*print_endline ("Unifying " ^ pp_ty t1 ^ " with " ^ pp_ty t2); *)
    match (t1,t2) with 
    | TArr (l1,l2) , TArr (r1,r2) -> 
        let s1 = unify l1 r1 in 
        let s2 = unify (apply_ty s1 l2) (apply_ty s1 r2) in 
            compose s2 s1
    
    | TVar (TV (x,Universal)) , t -> bind x t
    | t , TVar (TV (x,Universal)) -> bind x t 

    | TVar (TV (x,Skolem)) , TCon _ -> bind x t2 
    | TCon _ , TVar (TV (x,Skolem)) -> bind x t1 
    
    | TVar (TV (x,Skolem)) , TVar (TV (y,Skolem)) -> if x=y then emptySubst else raise @@ Skolem (x,y)

    | TCon a , TCon b -> if a=b then emptySubst 
                                else raise @@ UnificationFailure (t1,t2) 
    | _ , _ -> raise @@ UnificationFailure (t1,t2)


    (* Generalization and Instantiation *)


    let instantiate = function
        Forall (xs,t) -> 
            let s = List.fold_left (fun acc x -> Env.add x (un_var (fresh_tvar ())) acc ) emptySubst xs in
                apply_ty s t

    
    (* Generalization converts a type into a scheme by closing over all free tvars *)
    let generalize env t = 
        let xs = Set.fold (fun x acc -> x::acc) (Set.diff (ftv_ty t) (ftv_tenv env)) [] in 
        Forall (xs,t)


    (* Hindley-Milner Type Inference *)
    exception UnboundVar of id
    exception NonHM

    let lookupEnv tenv x = 
        match Env.find_opt x tenv with 
        | None -> raise @@ UnboundVar x
        | Some sch -> (emptySubst, instantiate sch)

    let ops = function
        | Add  
        | Mul 
        | Sub -> TArr (typeInt, TArr (typeInt,typeInt))
        | Eql -> TArr (typeInt, TArr (typeInt,typeBool))

    let rec infer_ty tenv expr =
        match expr with 
        | Var x -> lookupEnv tenv x

        (*    x:a |-   e : t2      *)
        (*   Lam(x,e) :  t1 -> t2  *)

        | Lam (x,e) -> 
            let t1 = un_var (fresh_tvar ()) in 
            let tenv' = Env.add x (Forall ([],t1)) tenv in 
            let (s,t2) = infer_ty tenv' e in 
                (s, apply_ty s (TArr (t1,t2)))
        | App (e1,e2) -> 

         (* 
               Env |- e1 : (T1,s1)       Env[s1] |-  e2 : (T2,s2)       T1 = T2 -> T      
                Env |- App(e1,e2) : T ,    
          *)
            let t = un_var (fresh_tvar ()) in 
            let (s1,t1) = infer_ty tenv e1 in 
            let (s2,t2) = infer_ty (apply_tenv s1 tenv) e2 in 
            let s3 = unify (apply_ty s2 t1) (TArr (t2,t)) in 
                (compose s3 (compose s2 s1), apply_ty s3 t)
        | Let (x,e1,e2) ->
            let (s1,t1) = infer_ty tenv e1 in 
            let tenv' = apply_tenv s1 tenv in 
            let t1' = generalize tenv' t1 in (* Let-generalization *)
            let (s2,t2) = infer_ty (Env.add x t1' tenv') e2 in 
                (compose s2 s1 , t2 )
        | If (e1,e2,e3) -> 
            let (s1,t1) = infer_ty tenv e1 in 
            let (s2,t2) = infer_ty tenv e2 in 
            let (s3,t3) = infer_ty tenv e3 in 
            let s4 = unify t1 typeBool in 
            let s5 = unify t2 t3 in 
             (compose s5 (compose s4 (compose s3 (compose s2 s1))), apply_ty s5 t2)
        | Fix e -> 
            let (s1,t1) = infer_ty tenv e in 
            let t = un_var (fresh_tvar ()) in  
            let s2 = unify (TArr (t,t)) t1 in 
            (s2 , apply_ty s1 t)
        | Op (op,e1,e2) ->  
            let (s1,t1) = infer_ty tenv e1 in
            let (s2,t2) = infer_ty tenv e2 in 
            let t = un_var (fresh_tvar ()) in 
            let s3 = unify (TArr (t1,TArr (t2,t))) (ops op) in 
                (compose s3 (compose s2 s1) , apply_ty s3 t)
        | Lit (LInt _) -> (emptySubst,typeInt)
        | Lit (LBool _) -> (emptySubst,typeBool)
        | _ -> raise NonHM



(* Note that in HM, declarations (or let bindings) are generalized as much as possible. 
   In the program: 

   let id x = x         id : forall a . a -> a
    
    b->b    b |-> bool

    c->c    c |-> int
     
   let g = (id true , id 0)

   The let-bound id is assigned the type scheme Forall [x] Var "x" and so can always be instantiated
   with fresh type variables at each call site. 

   This is in contrast with:

   let f g = let h = (g true) in (g 3) 

   which is ill-typed in HM, since the type of g is not generalized and so cannot be used as the
   polymorphic identity in the definition of f. 

   See the three terms below or concrete examples. idtype is the fully generalized type
   for the identity function. 
   
   fail raises a unification error since the function g is not sufficiently generalized.

   success does not, since let-generalization saves the day.

   success' just shows that types in the environment *must* be generalized.
*)

let idtype = let (s,t) = infer_ty Env.empty (Lam ("x",Var "x")) in generalize Env.empty @@ apply_ty s t

(* let fail = infer_ty Env.empty (Lam ("g",Let ("h", App(Var"g",Lit (LBool true)), App(Var"g",Lit (LInt 3)) ))) *) 

let success = infer_ty Env.empty (Let ("id", Lam ("x",Var "x") , 
                            Let ("h", App(Var"id",Lit (LBool true)) , App(Var"id",Lit (LInt 3))  )))
let success' = infer_ty (Env.singleton "id" idtype) (Let ("h", App(Var"id",Lit (LBool true)) , App(Var"id",Lit (LInt 3))  ))


(* Now for synthesis...

    It is useful to consider a variant of the HM algorithm above that separates the AST traversal from
    constraint solving/unification. Lets do that first:
*)



    (* A unification constraint is just a pair of types to be unified *)
    type constr = ty * ty 
    type unifier = ty_subst * constr list 

      (* Constraint Solver *)
          
    let debug_state = ref false

    let debug s = if !debug_state then print_endline s else ()

    let bind_synth x t = 
            if occursCheck x t then 
                    raise @@ InfiniteType (x,t)
               else 
                    Env.singleton x t


            

    let rec unify_synth t1 t2 = 
    debug ("Unifying " ^ pp_ty t1 ^ " with " ^ pp_ty t2);
    match (t1,t2) with 
    | TArr (l1,l2) , TArr (r1,r2) -> 
        let s1 = unify_synth l1 r1 in 
        let s2 = unify_synth (apply_ty s1 l2) (apply_ty s1 r2) in 
            compose s2 s1
     
        (* Handle variables explicitly *)
    | TVar (TV (x,Universal)) , TVar (TV (y,Universal)) -> if x=y then emptySubst else raise @@ UnificationFailure (t1,t2)
    | TVar (TV (x,Skolem)) , TVar (TV (y,Skolem)) -> if x=y then emptySubst else Env.singleton x (TVar (TV (y,Skolem)))

    | TVar (TV (x,Universal)) , TVar (TV (y,Skolem)) -> Env.singleton y t1
    | TVar (TV (y,Skolem)) , TVar (TV (x,Universal)) -> Env.singleton y t2

    | TVar (TV (x,Skolem)) , _ ->  bind_synth x t2 
    | _ , TVar (TV (x,Skolem)) ->    bind_synth x t1 
    
    
    | TCon a , TCon b -> if a=b then emptySubst 
                                else raise @@ UnificationFailure (t1,t2) 
    | _ , _ -> raise @@ UnificationFailure (t1,t2)


    let rec solver (subs,clist) : ty_subst =
        match clist with 
            [] -> subs 
        | (t1,t2)::cs ->  
            let s = unify_synth t1 t2 in 
            solver (compose s subs , List.map ( fun (t,t') -> (apply_ty s t, apply_ty s t') ) cs)
        
    let runSolver clist = solver (Env.empty,clist)


    let instantiate_synth = function
        Forall (xs,t) -> 
            let s = List.fold_left (fun acc x -> Env.add x (skolem_var (fresh_tvar ())) acc ) emptySubst xs in
                apply_ty s t

    
    (* Generalization converts a type into a scheme by closing over all free tvars *)
    let generalize_synth env t = 
        let xs = Set.fold (fun x acc -> x::acc) (Set.diff (ftv_ty t) (ftv_tenv env)) [] in 
        Forall (xs,t)


    let addConstr clist t1 t2 = (t1,t2)::clist 

    let lookupEnv env x = 
        match Env.find_opt x env with 
        | None -> raise @@ UnboundVar x
        | Some sch -> (instantiate_synth sch)

    let rec infer ctxt clist e =  
        match e with 
        | Lit (LInt _) -> (clist,typeInt)
        | Lit (LBool _) -> (clist,typeBool)
        
        | Var x -> (clist,lookupEnv ctxt x)

        | Lam (x,e) -> 
            let tv = un_var (fresh_tvar ()) in 
            let (c,t) = infer (Env.add x (Forall ([],tv)) ctxt) clist e in 
                (c,TArr (tv,t))
        
        | App (e1,e2) -> 
            let (c1,t1) = infer ctxt clist e1 in 
            let (c2,t2) = infer ctxt c1 e2 in 
            let tv = un_var (fresh_tvar ()) in 
            let c' = addConstr c2 t1 (TArr (tv,t2)) in
                (c',tv)
                
        | Let (x,e1,e2) ->
            let (c1,t1) = infer ctxt clist e1 in 
            let s = runSolver c1 in (* To generalize in let we need to solve first *)
            let ctxt' = apply_tenv s ctxt in 
            let sc = generalize ctxt' (apply_ty s t1) in 
            let (c2,t2) = infer (Env.add x sc ctxt') [] e2 in (* is it c1 or [] here? *)
             (c2,t2)
            
        | Fix e -> 
            let (c,t) = infer ctxt clist e in 
            let tv = un_var (fresh_tvar ()) in 
            let c' = addConstr c t (TArr (tv,tv)) in 
                (c',tv)
        
        | If (e1,e2,e3) ->
            let (c1,t1) = infer ctxt clist e1 in 
            let (c2,t2) = infer ctxt c1 e2 in 
            let (c3,t3) = infer ctxt c2 e3 in 
            let c' = addConstr (addConstr c3 t2 t3) t1 typeBool in 
                (c',t2)
        
        | Op(op,e1,e2) -> 
            let (c1,t1) = infer ctxt clist e1 in
            let (c2,t2) = infer ctxt c1 e2 in 
            let t = un_var (fresh_tvar ()) in 
            let c' = addConstr c2 (TArr (t1,TArr (t2,t))) (ops op) in 
                (c',t)
        | _ -> raise NonHM

     
     let inferExpr ctxt e = 
        let (c,t) = infer ctxt [] e in 
          let sub = runSolver c in 
            generalize Env.empty (apply_ty sub t)



    exception Backtrack
    exception Fail

    let rec invert_ty constrs gctxt lctxt = function 
       | TArr (t1,t2) -> 
            let x = fresh_id () in 
            let (c,e) = invert_ty constrs gctxt ((x,t1)::lctxt) t2 in 
                let s = runSolver c in 
                (* Necessary? *)
                (c,Lam (x,apply_tytm s e))
       | TCon _ | TVar _ as t -> 
            let (c,e) = focus1 constrs gctxt lctxt lctxt t in 
            let s = runSolver c in 
                (* Necessary? *)
                (c,apply_tytm s e)

    and focus1 constrs gctxt lctxt c goal = 
     match lctxt with
     | [] -> focus2 constrs gctxt c gctxt goal
     | (x,t)::xs -> try let (c,e) = focus' constrs gctxt c x t goal in 
                        let s = runSolver c in 
                        (* Necessary? *)
                        (c,apply_tytm s e)
                            
                    with Backtrack | UnificationFailure _ -> focus1 constrs gctxt xs c goal
    and focus2 constrs gctxt lctxt g goal =
        match gctxt with 
        | [] -> raise Backtrack 
        | (x,t)::xs -> try focusScheme constrs g lctxt x t goal 
                       with Backtrack | UnificationFailure _  -> focus2 constrs xs lctxt g goal 
   
   
   (* 
        f: forall a b . a -> b

                 a' -> b'   f[a',b'](x)
                                                a' = c'
                                           d'   b' = d'
                    Lam x:c' .  f[c',d'](x)           c' -> d'  



           Tabs ["c" "d"]  Lam x:c . f[c,d](x):  Forall c d . c -> d. 
    


             
   *)
   
    and focusScheme constrs gctxt lctxt id foc goal = 
        match foc with 
        | Forall (vars,t) -> 
            let params = List.map (fun _ -> skolem_var @@ fresh_tvar()) vars in
            (* inlined instantiation, but with skolem variables *) 
            let s = List.fold_left2 (fun acc x p -> Env.add x p acc ) emptySubst vars params in
            let inst_t = apply_ty s t in 
            let (c,e) = focus' constrs gctxt lctxt id inst_t goal in 
                let s' = runSolver c in 
                let e' = apply_tytm s' e in 
                (c,apply_tm (Env.singleton id (TApp (Var id, List.map (apply_ty s') params))) e')

    and focus' constrs gctxt lctxt id foc goal = 
        match foc with 
        | TArr (t1,t2) -> 
                    let (c1,e2) = focus' constrs gctxt lctxt id t2 goal in 
                    let (c2,e1) = invert_ty c1 gctxt lctxt t1 in 
                        (c2,apply_tm (Env.singleton id (App (Var id,e1))) e2)  

        | TCon a -> begin match goal with 
                            TCon b -> if a=b then (constrs,Var id) else raise Backtrack 
                          | TVar x -> (addConstr constrs foc goal, Var id)
                          | _ -> raise Fail
                    end

        | TVar x -> begin match goal with 
                           TCon b -> (addConstr constrs foc goal, Var id)
                        |  TVar y -> (addConstr constrs foc goal, Var id)
                        | _ -> raise Fail
                    end 
                           
               

   
        

    let rec invert_scheme ctxt = function
        (Forall (l,t)) -> 
            let params = List.map (fun _ -> fresh_tvar()) l in
            (* inlined instantiation *) 
            let s = List.fold_left2 (fun acc x p -> Env.add x (un_var p) acc ) emptySubst l params in
            let t_inst = apply_ty s t in 
            let (c,e) = invert_ty [] ctxt [] t_inst in
            let s' = runSolver c in (* Not sure about this bit... *)
            TLam (params, apply_tytm s' e)


    let idScheme = Forall (["a"],TArr (un_var "a", un_var "a"))
    let abort = Forall (["a"],un_var "a")

    let exIntQuodLibet = Forall (["a"], TArr (typeInt,un_var "a"))  

    let skolemNegativePositionTest1 = Forall (["a"], TArr (TArr (un_var "a",typeBool),typeInt))
    let skolemNegativePositionTest2 = Forall (["a"], TArr (TArr (un_var "a",typeInt),typeBool))

    (* f: forall a . (a -> Bool) -> Int *)

    (* g : Int -> Bool   *)



    let bad1 = TArr (typeInt,typeBool) (* fails in ctxt with the idScheme as needed *)
    let bad3 = TArr (typeBool,typeInt) 
    let bad2 = Forall (["b"],TArr (typeBool,un_var "b")) (* Must fail in the empty ctxt *)



end