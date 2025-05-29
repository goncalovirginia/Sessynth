open Language;;

open Z3;;
open Z3.Boolean;;
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

let x_tyA_to_exp z3ctxt x tA =
    let sym = Symbol.mk_string z3ctxt x in
    match tA with
    | TInt -> Integer.mk_const z3ctxt sym
    | TBool -> Boolean.mk_const z3ctxt sym

let x_tyA_to_expr_with_coefficient z3ctxt x tA =
    let sym = Symbol.mk_string z3ctxt x in
    match tA with
    | TInt -> 
        let coefficient_sym = Symbol.mk_string z3ctxt ("_" ^ x) in 
        mk_mul z3ctxt [Integer.mk_const z3ctxt coefficient_sym; Integer.mk_const z3ctxt sym]
    | TBool -> Boolean.mk_const z3ctxt sym

let tyA_to_sort z3ctxt tA =
    match tA with
    | TInt -> Integer.mk_sort z3ctxt
    | TBool -> Boolean.mk_sort z3ctxt

let tyR_to_expr p z3ctxt tR withCoefficients =
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
        | RTVar(x) -> (if withCoefficients then x_tyA_to_expr_with_coefficient else x_tyA_to_exp) z3ctxt x (get_x_tyA p x)
    in tyR_to_expr' z3ctxt tR

let psi_to_constraints p z3ctxt =
    List.filter_map(fun (x, t) -> 
        match t with 
        | TRefinement(_, _, RTBool(_)) -> None
        | TRefinement(x, tA, tR) -> 
            begin match tR with
            | RTEq(RTVar(x), tR') | RTGr(RTVar(x), tR') | RTGrE(RTVar(x), tR') | RTLt(RTVar(x), tR') | RTLtE(RTVar(x), tR') ->
                Some (tyR_to_expr p z3ctxt tR' false)
            | _ -> raise (Error "Invalid parameter refinement predicate")
            end
        | _ -> None
    ) p

let get_tyR_referenced_vars tR =
    let fresh_int_id = let unique = ref (-1) in fun () -> (incr unique; string_of_int !unique) in
    let hash_table = Hashtbl.create 10 in
    let rec traverse goal =
        match goal with
        | RTAnd(t1, t2) | RTOr(t1, t2) | RTEq(t1, t2) | RTGr(t1, t2) | RTLt(t1, t2) | RTGrE(t1, t2) | RTLtE(t1, t2) | RTSum(t1, t2) | RTSub(t1, t2) | RTMult(t1, t2) | RTDiv(t1, t2) -> 
            traverse t1;
            traverse t2
        | RTVar(x) -> 
            Hashtbl.add hash_table x (fresh_int_id())
        | _ -> ()
    in
    traverse tR;
    List.of_seq (Hashtbl.to_seq hash_table)

let get_vars_sorts xl p z3ctxt =
    List.map (fun x -> 
        let tA = get_x_tyA p x in
        tyA_to_sort z3ctxt tA
    ) xl

let construct_app_constraint_expr tR z3ctxt goal_app goal_expr =
    match tR with
    | RTEq _ -> mk_eq z3ctxt goal_app goal_expr
    | RTGr _ -> mk_gt z3ctxt goal_app goal_expr
    | RTLt _ -> mk_lt z3ctxt goal_app goal_expr
    | RTGrE _ -> mk_gt z3ctxt goal_app goal_expr
    | RTLtE _ -> mk_le z3ctxt goal_app goal_expr
    | _ -> raise (Error "construct_tyR: invalid function application predicate")

let goal_to_constraints goal p z3ctxt =
    match goal with
    | TRefinement(x, tA, tR) -> 
        begin match tR with
        | RTEq(RTVar(x), tR') | RTGr(RTVar(x), tR') | RTGrE(RTVar(x), tR') | RTLt(RTVar(x), tR') | RTLtE(RTVar(x), tR') ->
            let referenced_vars_ints = get_tyR_referenced_vars tR' in
            let referenced_vars = List.map fst referenced_vars_ints in
            let referenced_sorts = get_vars_sorts referenced_vars p z3ctxt in
            let referenced_vars_sorts = List.combine referenced_vars referenced_sorts in
            let goal_sort = tyA_to_sort z3ctxt tA in
            let goal_sym = Symbol.mk_string z3ctxt x in
            let goal_declr = FuncDecl.mk_func_decl z3ctxt goal_sym referenced_sorts goal_sort in
            let referenced_vars_expr = List.map(fun (x, s) -> Expr.mk_const z3ctxt (Symbol.mk_string z3ctxt x) s) referenced_vars_sorts in
            let goal_app = Expr.mk_app z3ctxt goal_declr referenced_vars_expr in
            let goal_expr = tyR_to_expr p z3ctxt tR' false in
            let goal_expr_with_coefficients = tyR_to_expr p z3ctxt tR' true in
            let goal_expr_with_coefficients = 
                if Sort.get_sort_kind (Expr.get_sort goal_expr_with_coefficients) = Z3enums.INT_SORT 
                then mk_add z3ctxt [goal_expr_with_coefficients; Integer.mk_const z3ctxt (Symbol.mk_string z3ctxt "_c")]
                else goal_expr_with_coefficients in
            let goal_app_eq_expr = construct_app_constraint_expr (RTEq(RTInt(-1), RTInt(-1))) z3ctxt goal_app goal_expr_with_coefficients in
            let goal_app_eq_forall_expr = Quantifier.expr_of_quantifier (Quantifier.mk_forall_const z3ctxt referenced_vars_expr goal_app_eq_expr None [] [] (Some goal_sym) None) in
            let goal_app_constraint_expr = construct_app_constraint_expr tR z3ctxt goal_app goal_expr in
            let goal_app_constraint_forall_expr = Quantifier.expr_of_quantifier (Quantifier.mk_forall_const z3ctxt referenced_vars_expr goal_app_constraint_expr None [] [] (Some goal_sym) None) in
            referenced_vars_ints, x, [goal_app_eq_forall_expr; goal_app_constraint_forall_expr]
        | _ -> raise (Error "Z3adapter: invalid goal refinement predicate")
        end
    | _ -> raise (Error "Z3adapter: goal is not of type TRefinement")

let expr_to_expF expr referenced_vars_ints =
    let rec expr_to_expF' expr =
        match AST.get_ast_kind (Expr.ast_of_expr expr) with
        | Z3enums.APP_AST ->
            let decl = Expr.get_func_decl expr in
            let name = FuncDecl.get_name decl |> Symbol.to_string in
            let args = Expr.get_args expr in
            begin match name, args with
            | "and", [a; b] -> And (expr_to_expF' a, expr_to_expF' b)
            | "or", [a; b] -> Or (expr_to_expF' a, expr_to_expF' b)
            | "=", [a; b] -> Eq (expr_to_expF' a, expr_to_expF' b)
            | ">", [a; b] -> Gr (expr_to_expF' a, expr_to_expF' b)
            | "<", [a; b] -> Lt (expr_to_expF' a, expr_to_expF' b)
            | ">=", [a; b] -> GrE (expr_to_expF' a, expr_to_expF' b)
            | "<=", [a; b] -> LtE (expr_to_expF' a, expr_to_expF' b)
            | "+", [a; b] -> Sum (expr_to_expF' a, expr_to_expF' b)
            | "+", [a; b; c] -> Sum (expr_to_expF' a, Sum (expr_to_expF' b, expr_to_expF' c))
            | "-", [a; b] -> Sub (expr_to_expF' a, expr_to_expF' b)
            | "*", [a; b] -> Mult (expr_to_expF' a, expr_to_expF' b)
            | "div", [a; b] -> Div (expr_to_expF' a, expr_to_expF' b)
            | _ -> raise (Error ("Unsupported expr: " ^ Expr.to_string expr))
            end
        | Z3enums.NUMERAL_AST -> 
            if Arithmetic.is_int expr then Int (int_of_string (Integer.numeral_to_string expr))
            else if Boolean.is_bool expr then Bool (expr |> Expr.to_string |> bool_of_string)
            else raise (Error ("Unsupported expr: " ^ Expr.to_string expr))
        | Z3enums.VAR_AST ->
            let var_index = string_of_int (Quantifier.get_index expr) in
            let var_id = fst (List.find (fun (_, v) -> v = var_index) referenced_vars_ints) in
            Var(var_id)
        | _ -> raise (Error ("Unsupported expr: " ^ Expr.to_string expr))
    in expr_to_expF' expr

let model_to_expF m goal_id referenced_vars_ints =
    let func_decls = Model.get_func_decls m in
    let rec find_f func_decls =
        begin match func_decls with
        | c::consts' ->
            let func_declr_id = Symbol.to_string (FuncDecl.get_name c) in
            if func_declr_id = goal_id then 
                let func_interp_option = Model.get_func_interp m c in
                begin match func_interp_option with
                | Some func_interp -> 
                    let expr = FuncInterp.get_else func_interp in
                    print_endline (Expr.to_string expr);
                    expr_to_expF expr referenced_vars_ints
                | None -> 
                    raise (Error ("No function with provided name " ^ goal_id ^ " was found in model"))
                end
            else find_f consts'
        | [] -> raise (Error ("No function with provided name " ^ goal_id ^ " was found in model"))
        end
     in find_f func_decls

let solve p goal =
    let z3ctxt = mk_context [] in
    let psi_constraints = psi_to_constraints p z3ctxt in
    let referenced_vars_ints, goal_id, goal_constraints = goal_to_constraints goal p z3ctxt in
    let constraints = psi_constraints@goal_constraints in
    List.iter (fun e -> print_endline (Expr.to_string e)) constraints;
    let s = Solver.mk_simple_solver z3ctxt in
    match Solver.check s constraints with
    | UNSATISFIABLE -> raise (Unsatisfiable "Expression is unsatisfiable")
    | UNKNOWN -> raise (Unknown (get_reason_unknown s))
    | SATISFIABLE -> 
        match get_model s with
        | Some m -> 
            print_endline (Z3.Model.to_string m);
            model_to_expF m goal_id referenced_vars_ints
        | None -> raise (Error "Expression is satisfiable but no model was returned")
