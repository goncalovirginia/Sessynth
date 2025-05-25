open Language;;

open Z3;;
open Z3.Symbol;;
open Z3.Sort;;
open Z3.Expr;;
open Z3.Boolean;;
open Z3.FuncDecl;;
open Z3.Goal;;
open Z3.Tactic;;
open Z3.Tactic.ApplyResult;;
open Z3.Probe;;
open Z3.Solver;;
open Z3.Arithmetic;;
open Z3.Arithmetic.Integer;;
open Z3.Model;;

exception Unsatisfiable of string
exception Unknown of string
exception Error of string

let get_x_tyA p x =
    match List.assoc x p with
    | TAtomic(tA) -> tA
    | TRefinement(_, tA, _) -> tA
    | _ -> raise (Error "Z3adapter only handles TAtomic and TRefinement")

let x_tyA_to_expr z3ctxt x tA =
    let sym = Symbol.mk_string z3ctxt x in
    match tA with
    | TInt -> Integer.mk_const z3ctxt sym
    | TBool -> Boolean.mk_const z3ctxt sym

let tyR_to_expr p z3ctxt tR =
    let rec tyR_to_expr' z3ctxt t =
        match t with
        | RTAnd(t1, t2) -> mk_and z3ctxt [tyR_to_expr' z3ctxt t1; tyR_to_expr' z3ctxt t2]
        | RTOr(t1, t2) -> mk_or z3ctxt [tyR_to_expr' z3ctxt t1; tyR_to_expr' z3ctxt t2]
        | RTEq(t1, t2) -> mk_eq z3ctxt (tyR_to_expr' z3ctxt t1) (tyR_to_expr' z3ctxt t2)
        | RTGr(t1, t2) -> mk_gt z3ctxt (tyR_to_expr' z3ctxt t1) (tyR_to_expr' z3ctxt t2)
        | RTLt(t1, t2) -> mk_lt z3ctxt (tyR_to_expr' z3ctxt t1) (tyR_to_expr' z3ctxt t2)
        | RTGrE(t1, t2) -> mk_ge z3ctxt (tyR_to_expr' z3ctxt t1) (tyR_to_expr' z3ctxt t2)
        | RTLtE(t1, t2) -> mk_le z3ctxt (tyR_to_expr' z3ctxt t1) (tyR_to_expr' z3ctxt t2)
        | RTSum(t1, t2) -> mk_add z3ctxt [tyR_to_expr' z3ctxt t1; tyR_to_expr' z3ctxt t2]
        | RTSub(t1, t2) -> mk_sub z3ctxt [tyR_to_expr' z3ctxt t1; tyR_to_expr' z3ctxt t2]
        | RTMult(t1, t2) -> mk_mul z3ctxt [tyR_to_expr' z3ctxt t1; tyR_to_expr' z3ctxt t2]
        | RTDiv(t1, t2) -> mk_div z3ctxt (tyR_to_expr' z3ctxt t1) (tyR_to_expr' z3ctxt t2)
        | RTInt(v) -> mk_numeral_i z3ctxt v
        | RTBool(v) -> if v then mk_true z3ctxt else mk_false z3ctxt
        | RTVar(x) -> x_tyA_to_expr z3ctxt x (get_x_tyA p x)
    in tyR_to_expr' z3ctxt tR

let x_tyF_to_expr p z3ctxt x tF =
    match tF with
    | TAtomic(tA) -> x_tyA_to_expr z3ctxt x tA
    | TRefinement(x, tA, tR) -> tyR_to_expr p z3ctxt tR
    | _ -> raise (Error "Z3adapter only handles TAtomic and TRefinement")

let rec psi_to_expr_list p z3ctxt =
    List.map(fun (x, t) -> x_tyF_to_expr p z3ctxt x t) p

let solve p goal =
    let ctxt = mk_context [] in
    let psi_expr_list = psi_to_expr_list p ctxt in
    let goal_expr = tyR_to_expr p ctxt goal in
    let constraints = goal_expr::psi_expr_list in
    let s = Solver.mk_simple_solver ctxt in
    match Solver.check s constraints with
    | UNSATISFIABLE -> raise (Unsatisfiable "Expression is unsatisfiable")
    | UNKNOWN -> raise (Unknown (get_reason_unknown s))
    | SATISFIABLE -> 
        match get_model s with
        | Some m -> m
        | None -> raise (Error "Expression is satisfiable but no model was returned")
