open Syntax
open Printer
open BundleMessages
(*Forward and recursion optimization *)

let logger = Logs.Src.create "Sessint"

let is_rec_decl = ref false

(** Checks if a spawn is used do spawn a recursive call of the defined function
  @param spawn_proc the process that is spawned
  @param decl_name the name of the initial declaration were the spawn is defined*)
let check_recursion spawn_proc decl_name =
  match spawn_proc with
  | FunApp (fun_name, _) -> (
      match fun_name with Var name -> name = decl_name | _ -> false)
  | _ -> false

(** Optimizes a given expression and returns an optimized representation of that expression. 
Patters to optimize: 
-> Spawn followed by a forward to the offered channel (d<-spawn M; fwd d c)
-> Identifies recursive function definitions and changes its type from FunDef to RecFunDef
    @param decl_name name of the declaration that contains the expression, allows fo the detection of recursive function calls
    @param expression expression to optimize  
    @param ctxt original names of channels channels used in the declaration*)
let rec optimize_exp decl_name expression ctxt arg =
  match expression with
  | BOp (op, e1, e2) ->
      BOp (op, optimize_exp decl_name e1 ctxt arg, optimize_exp decl_name e2 ctxt arg)
  | UOp (op, e) -> UOp (op, optimize_exp decl_name e ctxt arg)
  | Let (v, e1, e2) ->
      Let (v, optimize_exp decl_name e1 ctxt arg, optimize_exp decl_name e2 ctxt arg)
  | FunDef (arg_name, arg_type, body, ret) ->
      let new_exp = optimize_exp decl_name body ctxt (Some arg_name) in
   if !is_rec_decl then RecFunDef (arg_name, arg_type, new_exp, ret)
      else FunDef (arg_name, arg_type, new_exp, ret)
  | FunApp (e1, e2) ->
      FunApp (optimize_exp decl_name e1 ctxt arg, optimize_exp decl_name e2 ctxt arg)
  | Annot (e, fun_type) -> Annot (optimize_exp decl_name e ctxt arg, fun_type)
  | Cond (cond, e1, e2) ->
      Cond
        ( optimize_exp decl_name cond ctxt arg,
          optimize_exp decl_name e1 ctxt arg,
          optimize_exp decl_name e2 ctxt arg )
  | ProcExp (offered, proc, type_option, list) ->
      ProcExp (offered, optimize_proc decl_name proc ctxt arg, type_option, list)
  | ExecExp exp -> optimize_exp decl_name exp ctxt arg
  | _ -> expression (*(UnitVal|Num _|Bool _|Var _)*)

and optimize_proc decl_name proc ctxt arg =
  match proc with
  | Spawn (_, _, _, proc_spawn, _) ->
      optimize_spawn_forward decl_name proc_spawn proc ctxt arg
  | Send (v, e1, ty, proc) ->
      Send (v, optimize_exp decl_name e1 ctxt arg, ty, optimize_proc decl_name proc ctxt arg)
  | Recv (v1, v2, ty, proc) -> Recv (v1, v2, ty, optimize_proc decl_name proc ctxt arg)
  | Wait (v, proc) -> Wait (v, optimize_proc decl_name proc ctxt arg)
  | TailSpawn (d, exp, opt, args, is_recursive, arg, original_args) ->
      TailSpawn
        (d, optimize_exp decl_name exp ctxt arg, opt, args, is_recursive, arg, original_args)
  | Choice (choice, labels) -> Choice (choice, process_choice_list decl_name labels ctxt arg)
  | Label (v1, v2, proc, stype) -> Label (v1, v2, optimize_proc decl_name proc ctxt arg, stype)
  | SendChan (v1, v2, stype, proc) ->
      SendChan (v1, v2, stype, optimize_proc decl_name proc ctxt arg)
  | RecvChan (v1, v2, stype, proc) ->
      RecvChan (v1, v2, stype, optimize_proc decl_name proc ctxt arg)
  | Print (exp, proc) ->
      Print (optimize_exp decl_name exp ctxt arg, optimize_proc decl_name proc ctxt arg)
  | If (exp, proc1, proc2) ->
      If
        ( optimize_exp decl_name exp ctxt arg,
          optimize_proc decl_name proc1 ctxt arg,
          optimize_proc decl_name proc2 ctxt arg )
  | _ -> proc (*Close, Fwd (stype, c, d)*)

and process_choice_list decl_name list ctxt arg =
  match list with
  | (v, (proc, stype)) :: tail ->
      (v, (optimize_proc decl_name proc ctxt arg, stype))
      :: process_choice_list decl_name tail ctxt arg
  | [] -> []

(** After detecting a spawn we analise if it is possible to optimize the pattern  forward (fwd d c) after a spawn (d<-spawn). The optimization is still possible if there is a chain of process of type {!If} or {!Print} in between 
  @param proc_spawn process after the spawn
  @param spawn initial detected spawn 
  @param ctxt arg original names of channels channels used in the declaration  *)
and optimize_spawn_forward decl_name next spawn ctxt arg =
  match spawn with
  | Spawn (channel, exp, opt, _, args) -> (
      match next with
      | Fwd (st, offered_channel, amb_channel) ->
          (* Pattern to optimize the forward detected, perform optimization *)
          if amb_channel = channel then (
            Logs.debug (fun m ->
                m "Optimizing forward (%s) from spawn (%s)"
                  (Printer.string_from_proc (Fwd (st, offered_channel, amb_channel)))
                  (Printer.string_from_proc (Spawn (channel, exp, opt, next, args))));
            let is_recursive = check_recursion exp decl_name in
            Logs.debug (fun m -> m "Recursion detected: %b" is_recursive);
            (* Argument name it is not known at this point *)
            if is_recursive then
              (
                is_rec_decl := true;
                TailSpawn
                (offered_channel, exp, opt, args, is_recursive, arg, ctxt))
            else
            TailSpawn (offered_channel, exp, opt, args, is_recursive, None, []))
          else Spawn (channel, exp, opt, optimize_proc decl_name next ctxt arg, args)
      | Print (exp, proc) ->
          (* Pattern to optimize the forward (fwd d c) after a spawn (d<-spawn), Prints in
             the middle don't interfere with optimization *)
          Print (exp, optimize_spawn_forward decl_name proc spawn ctxt arg)
      | If (exp, proc1, proc2) ->
          (* Pattern to optimize the forward (fwd d c) after a spawn (d<-spawn), Ifs in
             the middle don't interfere with optimization *)
          If
            ( exp,
              optimize_spawn_forward decl_name proc1 spawn ctxt arg,
              optimize_spawn_forward decl_name proc2 spawn ctxt arg )
      | _ -> Spawn (channel, exp, opt, optimize_proc decl_name next ctxt arg, args))
  | _ -> spawn

let rec extract_channels_names ctxt =
  match ctxt with (name, _) :: tail -> name :: extract_channels_names tail | [] -> []

(**
    Computes a list of names of the original channels used during the declaration.
    This is only important if a function is declared in the declaration and incide of the function there is a process. 
    @param type_decl type of the declaration from which to extract the channel's names
    @return a list of channel's names used in the declaration*)
let original_channel_names type_decl =
  match type_decl with
  | TFun (_, body) -> (
      match body with TProc (_, ctxt) -> extract_channels_names ctxt | _ -> [])
  | _ -> []

let optimize_recursion_declarations declarations =
  let rec_functions = ref [] in
  let new_decls =
    List.fold_left
      (fun decls declaration ->
        match declaration with
        | Decl (name, type_decl, expression) ->
            is_rec_decl := false;
            let new_exp =
              optimize_exp name expression (original_channel_names type_decl) None
            in
            let is_rec = match new_exp with RecFunDef _ -> true | _ -> false in
            if is_rec then rec_functions := name :: !rec_functions;
            Decl (name, type_decl, new_exp) :: decls)
      [] declarations
  in
  (!rec_functions, new_decls)

(**Optimizes declarations regarding MultiSends and adds the informations of names of declarations that were optimized to a map - if a declaration was optimized we keep the original version an the optimized version 
  @param declarations list of declarations to optimize
  @param main main process to optimize 
  @param rec_functions list of names of recursive functions
  @return a tuple with the map of names of declarations that were optimized, the list of declarations optimized and the main process optimized  *)
let optimize_multisends declarations main rec_functions =
  let send_map = ref StringMap.empty in
  let new_decls =
    List.fold_left
      (fun decls declaration ->
        match declaration with
        | Decl (name, type_decl, exp) ->
            let multisends, new_exp =
              BundleMessages.bundle_send_exp exp name StringMap.empty rec_functions
            in
            if List.length multisends != 0 then (
              let opt_name = name ^ "_optimized" in
              send_map := StringMap.add opt_name multisends !send_map;
              let new_declaration = Decl (opt_name, type_decl, new_exp) in
              (* Add the old version and the optimized version *)
              declaration :: new_declaration :: decls)
            else declaration :: decls)
      [] declarations
  in
  let multisends, send_opt_main =
    BundleMessages.bundle_send_exp main "Main" StringMap.empty rec_functions
  in
  if List.length multisends != 0 then
    let new_send_map = StringMap.add "Main" multisends !send_map in
    (new_send_map, new_decls, send_opt_main)
  else (!send_map, new_decls, main)

(** Optimizes a list of declarations and a main expression to replace chains of Recv by MultiRecv, if there is a MultiSend found
  @param send_map map of found MultiSends in declarations
  @param declarations list of declarations to optimize
  @param main main process to optimize
  @return a tuple with the list of declarations optimized and the main process optimized  *)
let optimize_multireceives send_map declarations main =
  if BundleMessages.StringMap.is_empty send_map then (declarations, main)
  else
    let optimization_used = ref (List.map fst (StringMap.bindings send_map)) in
    let new_decls =
      List.fold_left
        (fun decls declaration ->
          match declaration with
          | Decl (name, type_decl, expression) ->
              Logs.debug (fun m -> m "Optimizing multireceive: %s" name);
              let was_opt, new_exp =
                BundleMessages.bundle_receive_exp expression send_map StringMap.empty name
              in
              if was_opt then (
                let opt_name = name ^ "_optimized" in
                (* Already processing an optimized version, save only the one optimized form, otherwise add both  version *)
                if String.ends_with ~suffix:"_optimized" name then
                  let new_decl = Decl (name, type_decl, new_exp) in
                  new_decl :: decls
                else
                  let new_decl = Decl (opt_name, type_decl, new_exp) in
                  optimization_used := opt_name :: !optimization_used;
                  declaration :: new_decl :: decls)
              else declaration :: decls)
        [] declarations
    in
    let was_opt, receive_opt_main =
      BundleMessages.bundle_receive_exp main send_map StringMap.empty "Main"
    in
    let opt_main =
      if was_opt then (
        optimization_used := "Main" :: !optimization_used;
        receive_opt_main)
      else main
    in
    Logs.debug (fun m ->
        m "Optimization used in : %s" (String.concat ", " !optimization_used));
    let corrected_types, corrected_main =
      BundleMessages.correct_types (List.rev new_decls) opt_main !optimization_used
    in
    (List.rev corrected_types, corrected_main)

(** Optimizes the intermediate representation of the program  
    @param prog previous intermediate representation, after desugaring*)
let optimize_representation prog =
  match prog with
  | Prog (declarations, main) ->
      let rec_functions, optimize_declarations =
        optimize_recursion_declarations declarations
      in
      let send_map, declarations_multisend, main_multisend =
        optimize_multisends optimize_declarations main rec_functions
      in
      Logs.debug (fun m ->
          m "Send map: %s" (BundleMessages.string_from_send_map send_map));
      if not (BundleMessages.StringMap.is_empty send_map) then (
        let declarations_multireceive, receive_opt_main =
          optimize_multireceives send_map declarations_multisend main_multisend
        in
        Prog (declarations_multireceive, receive_opt_main))
      else Prog (optimize_declarations, main)
