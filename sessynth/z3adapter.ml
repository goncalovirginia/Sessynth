open Language;;

open Z3;;
open Z3.Boolean;;
open Z3.Solver;;
open Z3.Arithmetic;;
open Z3.Arithmetic.Integer;;

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

let tyA_to_sort z3ctxt tA =
    match tA with
    | TInt -> Integer.mk_sort z3ctxt
    | TBool -> Boolean.mk_sort z3ctxt

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
        | RTBool(v) -> (if v then mk_true else mk_false) z3ctxt
        | RTVar(x) -> x_tyA_to_expr z3ctxt x (get_x_tyA p x)
    in tyR_to_expr' z3ctxt tR

let expr_to_expF expr =
    match expr with
    | _ -> "TODO"


let psi_to_constraints p z3ctxt =
    List.filter_map(fun (x, t) -> 
        match t with 
        | TRefinement(_, _, RTVar(_)) -> None
        | TRefinement(x, tA, tR) -> Some (tyR_to_expr p z3ctxt tR)
        | _ -> None
    ) p

let get_tyR_referenced_vars tR =
    let hash_table = Hashtbl.create 10 in
    let rec traverse goal =
        match goal with
        | RTAnd(t1, t2) | RTOr(t1, t2) | RTEq(t1, t2) | RTGr(t1, t2) | RTLt(t1, t2) | RTGrE(t1, t2) | RTLtE(t1, t2) | RTSum(t1, t2) | RTSub(t1, t2) | RTMult(t1, t2) | RTDiv(t1, t2) -> 
            traverse t1;
            traverse t2
        | RTVar(x) -> 
            Hashtbl.add hash_table x None
        | _ -> ()
    in
    traverse tR;
    List.of_seq (Hashtbl.to_seq_keys hash_table)

let get_vars_sorts xl p z3ctxt =
    List.map (fun x -> 
        let tA = get_x_tyA p x in
        tyA_to_sort z3ctxt tA
    ) xl

let goal_to_constraint goal p z3ctxt =
    match goal with
    | TRefinement(x, tA, tR) -> 
        let referenced_vars = get_tyR_referenced_vars tR in
        let referenced_sorts = get_vars_sorts referenced_vars p z3ctxt in
        let referenced_vars_sorts = List.combine referenced_vars referenced_sorts in
        let goal_sort = tyA_to_sort z3ctxt tA in
        let goal_sym = Symbol.mk_string z3ctxt x in
        let goal_declr = FuncDecl.mk_func_decl z3ctxt goal_sym referenced_sorts goal_sort in
        let referenced_vars_expr = List.map(fun (x, s) -> Expr.mk_const z3ctxt (Symbol.mk_string z3ctxt x) s) referenced_vars_sorts in
        let goal_app = Expr.mk_app z3ctxt goal_declr referenced_vars_expr in
        let goal_expr = tyR_to_expr p z3ctxt tR in
        let goal_forall_expr = Quantifier.expr_of_quantifier (Quantifier.mk_forall_const z3ctxt referenced_vars_expr goal_expr None [] [] (Some goal_sym) None) in
        goal_forall_expr
    | _ -> raise (Error "Z3adapter.solve: goal is not of type TRefinement")

let model_expr_to_expF v =
    if Arithmetic.is_int v then Int(int_of_string (Integer.numeral_to_string v))
    else if Boolean.is_bool v then Bool(bool_of_string (Expr.to_string v))
    else raise (Error "Unsuported generated model value")

let model_to_id_expF_list m =
    let consts = Model.get_const_decls m in
    List.map(fun c -> 
        let x = Symbol.to_string (FuncDecl.get_name c) in
        let v_option = Model.get_const_interp m c in
        match v_option with
        | Some v -> (x, model_expr_to_expF v)
        | None -> raise (Error "Somehow model variable does not have a value")
    ) consts

let solve p goal =
    let z3ctxt = mk_context [] in
    let psi_constraints = psi_to_constraints p z3ctxt in
    let goal_constraint = goal_to_constraint goal p z3ctxt in
    let constraints = goal_constraint::psi_constraints in
    List.iter (fun e -> print_endline (Expr.to_string e)) constraints;
    let s = Solver.mk_simple_solver z3ctxt in
    match Solver.check s constraints with
    | UNSATISFIABLE -> raise (Unsatisfiable "Expression is unsatisfiable")
    | UNKNOWN -> raise (Unknown (get_reason_unknown s))
    | SATISFIABLE -> 
        match get_model s with
        | Some m -> 
            print_endline (Z3.Model.to_string m);
            model_to_id_expF_list m
        | None -> raise (Error "Expression is satisfiable but no model was returned")
