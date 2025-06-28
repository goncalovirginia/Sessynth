open Syntax

module StringMap = Map.Make (String)
(** Saves the functions transformed to multisend and stores its new type*)

(** Stores the MultiSend found and distingues if it was found on the "main body" or in a local spawn *)
type found_multisends =
  | External of stype
      (** Indicates that a multisend was found associated with the original channel of a process*)
  | Internal of var option * stype
      (**InSpawn indicates that a MultiSend was found not on an original channel of a process, but in a channel created by a spawn -  if the Spawn is a FunApp the we save the function name, otherwise is we save the name of the declaration were the Spawn occurred *)
(*| InSpawn of var option * stype*)

(************************ Bundle messages Debug ***************************)

let get_name name = match name with Some n -> n | None -> "None"

(** Computes the string of the types in a StringMap - found_multisends
    @param s the found_multisend type *)
let string_from_found_multisends s =
  match s with
  | Internal (name, st) ->
      "InSpawn(" ^ get_name name ^ ", " ^ Printer.string_from_stype st ^ ")"
  | External st -> "InBody(" ^ Printer.string_from_stype st ^ ")"

(** Computes the string to represent a SendMap
      @param m the SendMap to process*)
let string_from_send_map m =
  let buf = Buffer.create 16 in
  StringMap.iter
    (fun k v ->
      Printf.bprintf buf "%s -> [" k;
      List.iter (fun x -> Printf.bprintf buf "%s; " (string_from_found_multisends x)) v;
      Printf.bprintf buf "]\n")
    m;
  Buffer.contents buf

(************************ Types***************************)

(**Checks if a type has an optimization of MultiSend or MultiReceive
  @param ty the type to check
  @return boolean indicating if a multisend or a multireceive type has found  *)
let rec has_optimized_ty ty =
  match ty with
  | TProc (st, _) -> has_optimized_st st
  | TFun (t1, t2) -> has_optimized_ty t1 || has_optimized_ty t2
  | TUnit | TNum | TBool | TVar _ -> false

(**Checks if a session type has an optimization of MultiSend or MultiReceive
  @param st the session type to check  
  @return boolean indicating if a multisend or a multireceive type has found*)
and has_optimized_st st =
  match st with
  | STMultiSend (_, _) | STMultiRecv (_, _) -> true
  | STEnd -> false
  | STSend (t, s) | STRecv (t, s) -> has_optimized_st s || has_optimized_ty t
  | STExtChoice st_list | STIntChoice st_list -> has_optimized_st_list st_list
  | STSendChan (s1, s2) | STRecvChan (s1, s2) ->
      has_optimized_st s1 || has_optimized_st s2
  | STVar _ | STRec (_, _) | STUVar _ -> false

and has_optimized_st_list st =
  match st with
  | [] -> false
  | (_, s) :: tail -> has_optimized_st s || has_optimized_st_list tail

(**Computes the type of a given process 
    @param original channel of the process
    @param proc process to  check*)
let rec get_type_proc ch proc =
  match proc with
  | Send (c, _, send_type, p) ->
      if ch = c then STSend (Option.get send_type, get_type_proc ch p)
      else get_type_proc ch p
  | Recv (_, c, recv_type, p) ->
      if ch = c then STRecv (Option.get recv_type, get_type_proc ch p)
      else get_type_proc ch p
  | MultiSend (c, _, types, p) ->
      if ch = c then
        STMultiSend (List.map (fun t -> Option.get t) types, get_type_proc ch p)
      else get_type_proc ch p
  | MultiRecv (c, _, types, p) ->
      if ch = c then
        STMultiRecv (List.map (fun t -> Option.get t) types, get_type_proc ch p)
      else get_type_proc ch p
  | Close _ -> STEnd
  | Wait (_, p) -> get_type_proc ch p
  | Fwd (_, _, _) -> STEnd
  | Spawn (_, _, _, p, _) -> get_type_proc ch p
  | TailSpawn (_, _, _, _, _, _, _) -> STEnd
  | Choice (_, l) -> STExtChoice (process_choice_list l)
  | Label (_, _, p, _) -> get_type_proc ch p
  | SendChan (_, _, st, p) -> STSendChan (Option.get st, get_type_proc ch p)
  | RecvChan (_, _, st, p) -> STRecvChan (Option.get st, get_type_proc ch p)
  | Print (_, p) -> get_type_proc ch p
  | If (_, _, _) -> STEnd

and process_choice_list l =
  match l with
  | [] -> []
  | (v, (_, t)) :: tail -> (v, Option.get t) :: process_choice_list tail

(**Computes the type of a given expression 
    @param exp expression to  check*)
and get_type_exp exp =
  match exp with
  | FunDef (_, arg_type, body, _) ->
      Some (TFun (Option.get arg_type, Option.get (get_type_exp body)))
  | RecFunDef (_, arg_type, _, ret) ->
      (* Does not change *)
      Some (TFun (Option.get arg_type, Option.get ret))
  | ProcExp (ch, proc, _, st_list) -> Some (TProc (get_type_proc ch proc, st_list))
  | Var v -> Some (TVar v)
  | UnitVal -> Some TUnit
  | Num _ -> Some TNum
  | Bool _ -> Some TBool
  | BOp (_, _, _) -> Some TNum
  | UOp (_, _) -> Some TBool
  | Let (_, _, _) | FunApp (_, _) | Annot (_, _) | Cond (_, _, _) | ExecExp _ -> None

(** Changes the type of an expression to replace chains of STSends and STRecv by STMultiSend and STMultiReceive
      @param decl_name name of the declaration where the expression is
      @param exp expression to change
      @param used_opt list of declaration names that used optimizations
      @param types_map new types of declarations - map where the key is the declaration name and the value is the new type
      @param ch_map map with the types of all the channels encountered - important to SendChan type*)
let rec change_types_exp decl_name exp used_opt types_map ch_map =
  match exp with
  | Var _ | UnitVal | Num _ | Bool _ -> exp
  | BOp (op, e1, e2) ->
      BOp
        ( op,
          change_types_exp decl_name e1 used_opt types_map ch_map,
          change_types_exp decl_name e2 used_opt types_map ch_map )
  | UOp (op, e) -> UOp (op, change_types_exp decl_name e used_opt types_map ch_map)
  | Let (v, e1, e2) ->
      Let
        ( v,
          change_types_exp decl_name e1 used_opt types_map ch_map,
          change_types_exp decl_name e2 used_opt types_map ch_map )
  | FunDef (v, arg, e, _) ->
      FunDef
        (v, arg, change_types_exp decl_name e used_opt types_map ch_map, get_type_exp e)
  | RecFunDef (_, _, _, _) -> exp
  | FunApp (e1, e2) ->
      FunApp
        ( change_types_exp decl_name e1 used_opt types_map ch_map,
          change_types_exp decl_name e2 used_opt types_map ch_map )
  | Annot (e, _) ->
      Annot
        ( change_types_exp decl_name e used_opt types_map ch_map,
          Option.get (get_type_exp e) )
  | Cond (c, t, e) ->
      Cond
        ( change_types_exp decl_name c used_opt types_map ch_map,
          change_types_exp decl_name t used_opt types_map ch_map,
          change_types_exp decl_name e used_opt types_map ch_map )
  | ProcExp (ch, proc, _, list_st) ->
      let new_type = get_type_proc ch proc in
      let new_ch_map = StringMap.add ch new_type ch_map in
      ProcExp
        ( ch,
          change_type_proc ch decl_name proc used_opt types_map new_ch_map,
          Some new_type,
          list_st )
  | ExecExp e -> ExecExp (change_types_exp decl_name e used_opt types_map ch_map)

and change_type_proc or_ch decl_name proc used_opt types_map ch_map =
  match proc with
  | Send (c, e, st, p) ->
      Send
        ( c,
          change_types_exp decl_name e used_opt types_map ch_map,
          st,
          change_type_proc or_ch decl_name p used_opt types_map ch_map )
  | Recv (c, v, ty, p) ->
      Recv (c, v, ty, change_type_proc or_ch decl_name p used_opt types_map ch_map)
  | MultiSend (_, _, _, _) | MultiRecv (_, _, _, _) -> proc
  | Close _ -> proc
  | Wait (c, p) -> Wait (c, change_type_proc or_ch decl_name p used_opt types_map ch_map)
  | Fwd (_, _, _) -> proc
  | Spawn (_, _, _, _, _) -> change_spawn or_ch decl_name proc used_opt types_map ch_map
  | TailSpawn (v, e, st, l, is_rec, arg, chs) ->
      if is_rec then proc (*Don't optimize multisend in recursion*)
      else
        let exp1 = change_types_exp decl_name e used_opt types_map ch_map in
        (*(detected, TailSpawn (v, exp1, st, args, b, arg, l))*)
        TailSpawn (v, exp1, st, l, is_rec, arg, chs)
  | Choice (v, list) ->
      Choice (v, change_type_choice or_ch decl_name list used_opt types_map ch_map)
  | Label (v1, v2, p, _) ->
      Label
        ( v1,
          v2,
          change_type_proc or_ch decl_name p used_opt types_map ch_map,
          Some (get_type_proc or_ch p) )
  | SendChan (ch, to_send, _, p) ->
      let ch_type = StringMap.find_opt ch ch_map in
      SendChan
        ( ch,
          to_send,
          ch_type,
          change_type_proc or_ch decl_name p used_opt types_map ch_map )
  | RecvChan (v, ch, st, p) ->
      RecvChan (v, ch, st, change_type_proc or_ch decl_name p used_opt types_map ch_map)
  | Print (e, p) -> Print (e, change_type_proc or_ch decl_name p used_opt types_map ch_map)
  | If (e, p1, p2) ->
      If
        ( e,
          change_type_proc or_ch decl_name p1 used_opt types_map ch_map,
          change_type_proc or_ch decl_name p2 used_opt types_map ch_map )

and change_type_choice or_ch decl_name list used_opt types_map ch_map =
  match list with
  | (v, (p, _)) :: tail ->
      let new_p = change_type_proc or_ch decl_name p used_opt types_map ch_map in
      let new_st = get_type_proc or_ch p in
      (v, (new_p, Some new_st))
      :: change_type_choice or_ch decl_name tail used_opt types_map ch_map
  | [] -> []

(** Modifies a spawn - if the process can use an optimized version and the Spawn is an !{FunApp} then calls the optimized version instead 
    @param or_ch original channel of the process
    @param decl_name name of the declaration where the expression is
    @param spawn spawn to change
    @param used_opt list of declaration names that used optimizations
    @param types_map new types of declarations - map where the key is the declaration name and the value is the new type
    @param ch_map map with the types of all the channels encountered*)
and change_spawn or_ch decl_name spawn used_opt types_map ch_map =
  match spawn with
  | Spawn (ch, exp, _, proc, list) -> (
      match exp with
      | FunApp (Var name, arg) ->
          (* If the spawn is a function application then we need to check if it uses the original version or the optimized version*)
          let opt_name = name ^ "_optimized" in
          let fun_app = StringMap.find_opt name types_map in
          let using_opt =
            (List.exists (fun x -> x = decl_name) used_opt
            && match fun_app with None -> false | Some _ -> true)
            ||
            (* The optimization din't change the type of the function so the optimization can be used freely*)
            match fun_app with None -> false | Some t -> not (has_optimized_ty t)
          in
          if using_opt then
            (* Check the type of the optimized version *)
            let new_type = StringMap.find_opt opt_name types_map in
            (* Extract the type to put on the type of the spawn *)
            let new_st =
              match new_type with
              | Some (TFun (_, TProc (body, _))) -> body
              | _ -> assert false
            in
            let new_ch_map = StringMap.add ch new_st ch_map in
            let e = change_types_exp decl_name arg used_opt types_map new_ch_map in
            (* Changes the call to the optimized version *)
            let new_exp = FunApp (Var opt_name, e) in
            let new_proc =
              change_type_proc or_ch decl_name proc used_opt types_map new_ch_map
            in
            Spawn (ch, new_exp, Some new_st, new_proc, list)
          else
            (* Does not used MultiSends optimizations but may have Multireceives so the type may change *)
            let new_type = StringMap.find name types_map in
            (* Extract the type to put on the type of the spawn *)
            let new_st =
              match new_type with TFun (_, TProc (body, _)) -> body | _ -> assert false
            in
            let new_ch_map = StringMap.add ch new_st ch_map in
            let new_exp = change_types_exp decl_name exp used_opt types_map new_ch_map in
            let new_proc =
              change_type_proc or_ch decl_name proc used_opt types_map new_ch_map
            in
            Spawn (ch, new_exp, Some new_st, new_proc, list)
      | _ ->
          (* Anonymous Spawn will have a TProc type *)
          let st =
            match get_type_exp exp with Some (TProc (st, _)) -> st | _ -> assert false
          in
          let new_ch_map = StringMap.add ch st ch_map in
          Spawn
            ( ch,
              change_types_exp decl_name exp used_opt types_map new_ch_map,
              Some st,
              change_type_proc or_ch decl_name proc used_opt types_map new_ch_map,
              list ))
  | _ -> assert false (*Only a spawn is passed*)

(** Corrects the types of one declaration
    @param types_map map with the new types of declarations
    @param used_optm list of declaration names that used optimizations
    @param decl declaration to correct
    @return a declaration with the correct types *)
let change_types_decl types_map used_optm decl =
  match decl with
  | Decl (name, t, expression) ->
      if String.ends_with ~suffix:"_optimized" name then (
        Logs.debug (fun m -> m "Correcting Decl: %s" name);
        let is_type = match t with TProc _ -> true | _ -> false in
        if is_type then decl
        else (
          types_map :=
            StringMap.add name (Option.get (get_type_exp expression)) !types_map;
          Decl
            ( name,
              Option.get (get_type_exp expression),
              change_types_exp name expression used_optm !types_map StringMap.empty )))
      else (
        (* original versions preserve original types *)
        types_map := StringMap.add name t !types_map;
        decl)

(** Corrects the types od a list of declarations and a main expression
@param decl_list list of declarations to correct
@param main main expression to correct
@param used_optm list of declaration names that used optimizations *)
let correct_types decl_list main used_optm =
  let types_map = ref StringMap.empty in
  let decls = List.map (change_types_decl types_map used_optm) decl_list in
  let main_opt = change_types_exp "Main" main used_optm !types_map StringMap.empty in
  (decls, main_opt)

(*************************** Bundle Send messages optimization***************************)

(**  Checks for a continuos chain of sends, to pack all the Sends in a MultiSend
@param proc first send of the chain
@param channel where the sends are being made (must be all the same in the chain of sends to transform in a MultiSend)
@returns the next process following the sends, a list with all the expressions in the Send and a list with all the types do send *)
let rec pack_sends proc channel =
  match proc with
  | Send (c, to_send, send_type, next) ->
      if c = channel then
        let new_next, list_sends, list_types = pack_sends next channel in
        (new_next, to_send :: list_sends, send_type :: list_types)
      else (proc, [], []) (*channel is not the same - break chain*)
  | _ -> (proc, [], [])

(** Iterates the process tree to replace chains of Send constructor with MultiSend's
@param proc process where to look for the chain of sends
@param original_channel original channel of the process
@param decl_name name of the declaration where the process is 
@param ctxt_channels map with channels with an FunApp associated to it (to save the function name if the MultiSend occurs associated to a Spawn)
@param rec_fun list with the recursive function (a recursive function can't be optimized)
@return a tuple: first a list of multisend founded and second the new process *)
let rec bundle_send_proc proc original_channel decl_name ctxt_channels rec_fun =
  match proc with
  | Send (channel, to_send, s_type, next) ->
      (* MultiSend only made if the Sends are in the original channel or occur associated  with a spawn in an allowed channel - only the allowed channels were added to the ctxt_channels*)
      Logs.debug (fun m ->
          m "Checking Send in channel %s, original channel %s. In declaration: %s" channel
            original_channel decl_name);
      if StringMap.mem channel ctxt_channels || channel = original_channel then
        let next_proc, list_send, types = pack_sends proc channel in
        if List.length list_send > 1 then
          (* MultiSend detected *)
          let multi_type =
            STMultiSend
              ( List.map (fun t -> Option.get t) types,
                get_type_proc original_channel next_proc )
          in
          let found_multisend =
            if original_channel = channel then External multi_type
            else
              let associated_fun =
                match StringMap.find_opt channel ctxt_channels with
                | None -> decl_name
                | Some v -> v
              in
              Internal (Some associated_fun, multi_type)
          in
          let detected, new_next =
            bundle_send_proc next_proc original_channel decl_name ctxt_channels rec_fun
          in
          (found_multisend :: detected, MultiSend (channel, list_send, types, new_next))
        else
          (* No multisend detected *)
          let detected, new_next =
            bundle_send_proc next original_channel decl_name ctxt_channels rec_fun
          in
          (detected, Send (channel, to_send, s_type, new_next))
      else
        (* No multisend detected *)
        let detected, new_next =
          bundle_send_proc next original_channel decl_name ctxt_channels rec_fun
        in
        (detected, Send (channel, to_send, s_type, new_next))
  | Recv (v1, v2, ty, proc) ->
      let detected, new_next =
        bundle_send_proc proc original_channel decl_name ctxt_channels rec_fun
      in
      (detected, Recv (v1, v2, ty, new_next))
  | Close _ -> ([], proc)
  | Wait (v, proc) ->
      let detected, new_next =
        bundle_send_proc proc original_channel decl_name ctxt_channels rec_fun
      in
      (detected, Wait (v, new_next))
  | Fwd (_, _, _) -> ([], proc)
  | Spawn (v, exp, st, proc, v_list) ->
      let new_channels =
        match exp with
        | FunApp (Var name, _) ->
            (*If the function is recursive we not allow allow bundle optimizations*)
            if List.exists (fun x -> x = name) rec_fun then ctxt_channels
            else StringMap.add v name ctxt_channels
        | _ ->
            (*Anonymous spawns stay associated with the declaration*)
            StringMap.add v decl_name ctxt_channels
      in
      let detected, exp1 = bundle_send_exp exp decl_name new_channels rec_fun in
      let detected2, new_proc =
        bundle_send_proc proc original_channel decl_name new_channels rec_fun
      in
      (detected @ detected2, Spawn (v, exp1, st, new_proc, v_list))
  | TailSpawn (v, e, st, l, is_rec, arg, chs) ->
      if is_rec then ([], proc) (*Don't optimize multisend in recursion*)
      else
        let new_channels =
          match e with
          | FunApp (Var name, _) ->
              (*If the function is recursive we not allow allow bundle optimizations*)
              if List.exists (fun x -> x = name) rec_fun then ctxt_channels
              else StringMap.add v name ctxt_channels
          | _ ->
              (*Anonymous spawns stay associated with the declaration*)
              StringMap.add v decl_name ctxt_channels
        in
        let detected, exp1 = bundle_send_exp e decl_name new_channels rec_fun in
        (detected, TailSpawn (v, exp1, st, l, is_rec, arg, chs))
  | Choice (ch, list) ->
      let detected, new_list =
        bundle_send_choice_list list original_channel decl_name ctxt_channels rec_fun
      in
      (detected, Choice (ch, new_list))
  | Label (v1, v2, proc, st) ->
      let detected, new_proc =
        bundle_send_proc proc original_channel decl_name ctxt_channels rec_fun
      in
      (detected, Label (v1, v2, new_proc, st))
  | SendChan (v1, v2, st, proc) ->
      let detected, new_proc =
        bundle_send_proc proc original_channel decl_name ctxt_channels rec_fun
      in
      (detected, SendChan (v1, v2, st, new_proc))
  | RecvChan (v1, v2, st, proc) ->
      let detected, new_proc =
        bundle_send_proc proc original_channel decl_name ctxt_channels rec_fun
      in
      (detected, RecvChan (v1, v2, st, new_proc))
  | Print (exp, proc) ->
      let detected, new_proc =
        bundle_send_proc proc original_channel decl_name ctxt_channels rec_fun
      in
      (detected, Print (exp, new_proc))
  | If (exp, proc1, proc2) ->
      let detected, new_exp1 = bundle_send_exp exp decl_name ctxt_channels rec_fun in
      let detected1, new_proc1 =
        bundle_send_proc proc1 original_channel decl_name ctxt_channels rec_fun
      in
      let detected2, new_proc2 =
        bundle_send_proc proc2 original_channel decl_name ctxt_channels rec_fun
      in
      (detected @ detected1 @ detected2, If (new_exp1, new_proc1, new_proc2))
  | _ -> ([], proc)
(*Multisend and multireceive don't occur*)

(** Checks for a multisend in a process choice list 
    @param list choice list
    @param channel original channel of the declaration
    @param decl_name name of the declaration 
    @return a list with the multisends found and the processed choice list*)
and bundle_send_choice_list list channel decl_name ctxt_channels rec_fun =
  match list with
  | (v, (proc, st)) :: tail ->
      let detected, new_proc =
        bundle_send_proc proc channel decl_name ctxt_channels rec_fun
      in
      let detected2, new_list =
        bundle_send_choice_list tail channel decl_name ctxt_channels rec_fun
      in
      (detected @ detected2, (v, (new_proc, st)) :: new_list)
  | [] -> ([], [])

(** Iterates the expression tree to replace chains of Sends constructor with MultiSends
    @param exp expression to process
    @param decl_name name of the declaration, or Main if it is the main process
    @param ctxt_channels map of the channels in the current context that can support multisend
    @param rec_fun list with the recursive function (a recursive function can't be optimized)
    @return a tuple with a list of the multisend found and the new expression *)
and bundle_send_exp exp decl_name ctxt_channels rec_fun =
  match exp with
  | Num _ | UnitVal | Bool _ | Var _ -> ([], exp)
  | BOp (op, e1, e2) ->
      let detected1, new_e1 = bundle_send_exp e1 decl_name ctxt_channels rec_fun in
      let detected2, new_e2 = bundle_send_exp e2 decl_name ctxt_channels rec_fun in
      (detected1 @ detected2, BOp (op, new_e1, new_e2))
  | UOp (op, e) ->
      let detected, new_e = bundle_send_exp e decl_name ctxt_channels rec_fun in
      (detected, UOp (op, new_e))
  | Let (v, e1, e2) ->
      let detected, new_e1 = bundle_send_exp e1 decl_name ctxt_channels rec_fun in
      let detected2, new_e2 = bundle_send_exp e2 decl_name ctxt_channels rec_fun in
      (detected @ detected2, Let (v, new_e1, new_e2))
  | FunDef (v, ty1, e, ty2) ->
      let detected, new_e = bundle_send_exp e decl_name ctxt_channels rec_fun in
      (detected, FunDef (v, ty1, new_e, ty2))
  | RecFunDef (_, _, _, _) ->
      (* Don't optimize mutisends in recursion *)
      ([], exp)
  | FunApp (e1, e2) ->
      let detected, new_e1 = bundle_send_exp e1 decl_name ctxt_channels rec_fun in
      let detected1, new_e2 = bundle_send_exp e2 decl_name ctxt_channels rec_fun in
      (detected @ detected1, FunApp (new_e1, new_e2))
  | Annot (e, t) ->
      let detected, new_e = bundle_send_exp e decl_name ctxt_channels rec_fun in
      (detected, Annot (new_e, t))
  | Cond (e1, e2, e3) ->
      let detected1, new_e1 = bundle_send_exp e1 decl_name ctxt_channels rec_fun in
      let detected2, new_e2 = bundle_send_exp e2 decl_name ctxt_channels rec_fun in
      let detected3, new_e3 = bundle_send_exp e3 decl_name ctxt_channels rec_fun in
      (detected1 @ detected2 @ detected3, Cond (new_e1, new_e2, new_e3))
  | ProcExp (ch, proc, st, list) ->
      let detected, new_proc = bundle_send_proc proc ch decl_name ctxt_channels rec_fun in
      (detected, ProcExp (ch, new_proc, st, list))
  | ExecExp e ->
      let detected, new_e = bundle_send_exp e decl_name ctxt_channels rec_fun in
      (detected, ExecExp new_e)

(*************************** Bundle Receive messages optimization***************************)

(** Changes the process tree so that the next of a process is updated to a new next
    @param new_next new next of the process
    @param proc process to change
    @return the new process with the new next
*)
let redefine_next new_next proc =
  match proc with
  | Recv (v, ch, receive_type, _) -> Recv (v, ch, receive_type, new_next)
  | Send (ch, e, send_type, _) -> Send (ch, e, send_type, new_next)
  | MultiSend (ch, list, types, _) -> MultiSend (ch, list, types, new_next)
  | MultiRecv (ch, list, types, _) -> MultiRecv (ch, list, types, new_next)
  | Wait (e, _) -> Wait (e, new_next)
  | Spawn (ch, proc, st, _, chs) -> Spawn (ch, proc, st, new_next, chs)
  | Label (v1, v2, _, st) -> Label (v1, v2, new_next, st)
  | SendChan (ch, e, send_type, _) -> SendChan (ch, e, send_type, new_next)
  | RecvChan (v, ch, receive_type, _) -> RecvChan (v, ch, receive_type, new_next)
  | Print (e, _) -> Print (e, new_next)
  | Close _
  | Fwd (_, _, _)
  | TailSpawn (_, _, _, _, _, _, _)
  | Choice (_, _)
  | If (_, _, _) ->
      proc

(** Modifies the tree of process to be a chain of the processes in the given list
    @param list of process to intercalate
    @return a process with a correct chain of continuation processes, without the receives bundled*)
let remove_receives list =
  let new_list = List.rev list in
  let first = List.hd new_list in
  List.fold_left redefine_next first (List.tl new_list)

(** Changes the name of variables associates that originate in a Multireceive, to use the same name in all the branches
  @param vars list of variables to change, that are associated with a multireceive (and the corresponding index)
  @param exp expression to change*)
let rec rename_variables_exp vars exp =
  match exp with
  | Var x -> (
      let xvar = List.find_opt (fun (_, y) -> y = x) vars in
      match xvar with None -> exp | Some (i, _) -> Var ("var_multi_" ^ string_of_int i))
  | UnitVal | Num _ | Bool _ -> exp
  | BOp (op, e1, e2) ->
      BOp (op, rename_variables_exp vars e1, rename_variables_exp vars e2)
  | UOp (op, e1) -> UOp (op, rename_variables_exp vars e1)
  | Let (v, e1, e2) -> Let (v, rename_variables_exp vars e1, rename_variables_exp vars e2)
  | FunDef (_, _, _, _)
  | RecFunDef (_, _, _, _)
  | FunApp (_, _)
  | Annot (_, _)
  | Cond (_, _, _)
  | ProcExp (_, _, _, _)
  | ExecExp _ -> exp

(** Changes the name of variables associates that originate in a Multireceive, to use the same name in all the branches
  @param list_variables list of variables to change, that are associated with a multireceive (and the corresponding index)
  @param proc process to change*)
and rename_variables_proc list_variables proc =
  match proc with
  | Recv (v, ch, receive_type, next) ->
      let new_next = rename_variables_proc list_variables next in
      Recv (v, ch, receive_type, new_next)
  | Send (ch, exp, send_type, next) ->
      let new_exp = rename_variables_exp list_variables exp in
      let new_next = rename_variables_proc list_variables next in
      Send (ch, new_exp, send_type, new_next)
  | MultiSend (ch, sends, types, next) ->
      let to_send = List.map (rename_variables_exp list_variables) sends in
      let new_next = rename_variables_proc list_variables next in
      MultiSend (ch, to_send, types, new_next)
  | MultiRecv (ch, vars, types, next) ->
      let new_next = rename_variables_proc list_variables next in
      MultiRecv (ch, vars, types, new_next)
  | Close _ -> proc
  | Wait (ch, next) ->
      let new_next = rename_variables_proc list_variables next in
      Wait (ch, new_next)
  | Fwd (_, _, _) -> proc
  | Spawn (ch, exp, st, next, args) ->
      (*The variables in {exp} are in a different context *)
      let new_next = rename_variables_proc list_variables next in
      Spawn (ch, exp, st, new_next, args)
  | TailSpawn (ch, exp, st, chs, is_rec, l1, l2) ->
      let new_exp = rename_variables_exp list_variables exp in
      TailSpawn (ch, new_exp, st, chs, is_rec, l1, l2)
  | Choice (v, list) ->
      let new_list = rename_vars_in_choice list list_variables in
      Choice (v, new_list)
  | Label (v1, v2, next, st) ->
      let new_next = rename_variables_proc list_variables next in
      Label (v1, v2, new_next, st)
  | SendChan (v1, v2, st, proc) ->
      let new_proc = rename_variables_proc list_variables proc in
      SendChan (v1, v2, st, new_proc)
  | RecvChan (v1, v2, st, proc) ->
      let new_proc = rename_variables_proc list_variables proc in
      SendChan (v1, v2, st, new_proc)
  | Print (exp, next) ->
      let new_exp = rename_variables_exp list_variables exp in
      let new_next = rename_variables_proc list_variables next in
      Print (new_exp, new_next)
  | If (exp, p1, p2) ->
      let new_exp = rename_variables_exp list_variables exp in
      let new_p1 = rename_variables_proc list_variables p1 in
      let new_p2 = rename_variables_proc list_variables p2 in
      If (new_exp, new_p1, new_p2)

and rename_vars_in_choice list list_variables =
  match list with
  | [] -> []
  | (v, (proc, st)) :: t ->
      let new_proc = rename_variables_proc list_variables proc in
      (v, (new_proc, st)) :: rename_vars_in_choice t list_variables

(**  Checks for a continuos chain of receives, to pack all the receives in a MultiReceive
@param proc first Receive of the chain
@param channel where the receives are being made (must be all the same in the chain of receives to transform in a MultiSend)
@param types list with the types of the receives (to match the corresponding sends)
@param size number of receives to pack (to match the corresponding sends)
@returns the next process following the receives, a list with the names of the variables used to stores the values and a list with all the types do Receive *)
let rec pack_receives proc channel types size =
  match proc with
  | Recv (v, ch, receive_type, next) ->
      if ch = channel && size > 0 then
        let n = List.length types - size in
        if Option.get receive_type = List.nth types n then
          (* matching type *)
          let list_variables, list_types, others =
            pack_receives next channel types (size - 1)
          in
          (v :: list_variables, receive_type :: list_types, others)
        else assert false (*types don't match, something is wrong*)
      else
        let list_variables, list_types, others = pack_receives next channel types size in
        (list_variables, list_types, proc :: others)
        (*channel is not the same - break chain*)
  | Print (_, next)
  | Send (_, _, _, next)
  | MultiSend (_, _, _, next)
  | MultiRecv (_, _, _, next)
  | Wait (_, next)
  | Spawn (_, _, _, next, _)
  | SendChan (_, _, _, next)
  | RecvChan (_, _, _, next)
  | Label (_, _, next, _) ->
      let list_variables, list_types, others = pack_receives next channel types size in
      (list_variables, list_types, proc :: others)
  | Close _ | Fwd (_, _, _) | TailSpawn (_, _, _, _, _, _, _) -> ([], [], [ proc ])
  | Choice (v, list) ->
      let types, new_list = pack_receives_choice list channel types size in
      let vars = List.mapi (fun i _ -> "var_multi_" ^ string_of_int i) types in
      (vars, types, [ Choice (v, new_list) ])
  | If (e1, p1, p2) ->
      let new_proc1, list_types1 = modify_branch_proc p1 channel types size in
      (* The types must be the same in all the branches *)
      let new_proc2, _ = modify_branch_proc p2 channel types size in
      let vars = List.mapi (fun i _ -> "var_multi_" ^ string_of_int i) list_types1 in
      (vars, list_types1, [ If (e1, new_proc1, new_proc2) ])

(** The number of receives must be the same in each branch, so we collect the types. The variables may have different names so we rename the variables, and remove the packed receives from the branch 
    @param list list of branches in the choice 
    @param channel where to check the multireceive
    @param types list of types of the associated multisend
    @param size number o receives left to pack
    @param list of types of receives encountered and the processed list of branches *)
and pack_receives_choice list channel types size =
  match list with
  | [] -> ([], [])
  | (v, (proc, _)) :: tail ->
      let new_proc, list_types = modify_branch_proc proc channel types size in
      let new_type = get_type_proc channel new_proc in
      let _, new_list = pack_receives_choice tail channel types size in
      (* The types of the receives must be equal in all branches *)
      (list_types, (v, (new_proc, Some new_type)) :: new_list)

(** we collect the types. The variables may have different names so we rename the variables, and remove the packed receives from the branch 
    @param proc beginning of the branch
    @param channel where to check the multireceive
    @param types list of types of the associated multisend
    @param size number o receives left to pack*)
and modify_branch_proc proc channel types size =
  let vars, list_types, others = pack_receives proc channel types size in
  let new_proc = remove_receives others in
  let vars_used = List.mapi (fun i v -> (i, v)) vars in
  (rename_variables_proc vars_used new_proc, list_types)


  (** Collects all the multisends associated with a given function
      @param name function name
      @param send_map map with the multisends*)
let find_associated_multisend name send_map =
  let associated_sends =
    StringMap.filter
      (fun _ v ->
        let is_associated list =
          List.exists
            (fun t ->
              match t with
              | Internal (n, _) -> ( match n with None -> false | Some n -> n = name)
              | _ -> false)
            list
        in
        is_associated v)
      send_map
  in
  List.concat (List.map snd (StringMap.bindings associated_sends))

(** Iterates the expression tree to replace chains of Receives constructor with MultiReceives, if there is a matching MultiSend
    @param exp expression to process
    @param send_map a StringMap with the multisends found
    @param ctxt_channels list with the channels where MultiSend were made (it is built during iteration of the expression tree)
    @param decl_name name of the declaration, or Main 
    @return a tuple with boolean indicating if the MultiReceive was used and the new expression *)
let rec bundle_receive_exp exp send_map ctxt_channels decl_name =
  match exp with
  | Num _ | UnitVal | Bool _ | Var _ -> (false, exp)
  | BOp (op, e1, e2) ->
      let was_opt1, new_e1 = bundle_receive_exp e1 send_map ctxt_channels decl_name in
      let was_opt2, new_e2 = bundle_receive_exp e2 send_map ctxt_channels decl_name in
      (was_opt1 || was_opt2, BOp (op, new_e1, new_e2))
  | UOp (op, e) ->
      let was_opt, new_e = bundle_receive_exp e send_map ctxt_channels decl_name in
      (was_opt, UOp (op, new_e))
  | Let (v, e1, e2) ->
      let was_opt1, new_e1 = bundle_receive_exp e1 send_map ctxt_channels decl_name in
      let was_opt2, new_e2 = bundle_receive_exp e2 send_map ctxt_channels decl_name in
      (was_opt1 || was_opt2, Let (v, new_e1, new_e2))
  | FunDef (arg, ty1, e, ty2) ->
      (* Look for a multisend associated with this function in the SendMap  - multisend made in a channel created by a spawn of this function*)
      let multisends = find_associated_multisend decl_name send_map in
      let new_ctxt =
        match e with
        | ProcExp (ch, _, _, _) -> (
            match multisends with
            | [] -> ctxt_channels
            | _ -> StringMap.add ch multisends ctxt_channels)
        | _ -> ctxt_channels
      in
      let was_opt, new_e = bundle_receive_exp e send_map new_ctxt decl_name in
      (was_opt, FunDef (arg, ty1, new_e, ty2))
  | RecFunDef (_, _, _, _) ->
      (* Don't optimize multisends in recursion *)
      (false, exp)
  | FunApp (e1, e2) ->
      let was_opt1, new_e1 = bundle_receive_exp e1 send_map ctxt_channels decl_name in
      let was_opt2, new_e2 = bundle_receive_exp e2 send_map ctxt_channels decl_name in
      (was_opt1 || was_opt2, FunApp (new_e1, new_e2))
  | Annot (e, t) ->
      let was_opt, new_e = bundle_receive_exp e send_map ctxt_channels decl_name in
      (was_opt, Annot (new_e, t))
  | Cond (e1, e2, e3) ->
      let opt1, new_e1 = bundle_receive_exp e1 send_map ctxt_channels decl_name in
      let opt2, new_e2 = bundle_receive_exp e2 send_map ctxt_channels decl_name in
      let opt3, new_e3 = bundle_receive_exp e3 send_map ctxt_channels decl_name in
      (opt1 || opt2 || opt3, Cond (new_e1, new_e2, new_e3))
  | ProcExp (ch, proc, st, list) ->
      let was_opt, new_proc = bundle_receive_proc proc send_map ctxt_channels decl_name in
      (was_opt, ProcExp (ch, new_proc, st, list))
  | ExecExp e ->
      let was_opt, new_e = bundle_receive_exp e send_map ctxt_channels decl_name in
      (was_opt, ExecExp new_e)

(** Iterates the processes tree to replace chains of Receives constructor with Multireceive's, if it matches a Multisend
  @param proc process to process
  @param send_map a StringMap with the multisends found
  @param ctxt_channels list with the channels where MultiSend were made (it is built during iteration of the expression tree)
  @param fun_name name of the declaration, or Main 
  @return a tuple with boolean indicating if the MultiReceive was used and the new expression *)
and bundle_receive_proc proc send_map channels_ctxt fun_name =
  match proc with
  | Send (channel, to_send, s_type, next) ->
      let was_opt, new_next = bundle_receive_proc next send_map channels_ctxt fun_name in
      (was_opt, Send (channel, to_send, s_type, new_next))
  | Recv (v1, ch, ty, p) -> (
      Logs.debug (fun m ->
          m "Recv in channel %s, with map: %s" ch (string_from_send_map channels_ctxt));
      match StringMap.find_opt ch channels_ctxt with
      | None ->
          (* No matching multisends*)
          let was_opt, new_next = bundle_receive_proc p send_map channels_ctxt fun_name in
          (was_opt, Recv (v1, ch, ty, new_next))
      | Some multisends ->
          let multisend = List.hd multisends in
          let n =
            match multisend with
            | External s | Internal (_, s) -> (
                match s with STMultiSend (l, _) -> List.length l | _ -> 0)
          in
          let types =
            match multisend with
            | External s | Internal (_, s) -> (
                match s with STMultiSend (l, _) -> l | _ -> [])
          in
          let vars, types, nexts = pack_receives proc ch types n in
          let new_proc = remove_receives nexts in
          (* Remove the used multisend from context *)
          let new_ctxt =
            if List.length multisends > 1 then
              let m = StringMap.remove ch channels_ctxt in
              StringMap.add ch (List.tl multisends) m
            else StringMap.remove ch channels_ctxt
          in
          let _, proc1 = bundle_receive_proc new_proc send_map new_ctxt fun_name in
          (* Add the new multisend to the context *)
          (true, MultiRecv (ch, vars, types, proc1)))
  | Close _ -> (false, proc)
  | Wait (v, proc) ->
      let was_opt, new_next = bundle_receive_proc proc send_map channels_ctxt fun_name in
      (was_opt, Wait (v, new_next))
  | Fwd (_, _, _) -> (false, proc)
  | Spawn (ch, exp, st, p, v_list) -> (
      match exp with
      | FunApp (Var name, _) -> (
          let opt_name = name ^ "_optimized" in
          match StringMap.find_opt opt_name send_map with
          | None -> (let was_opt, new_proc = bundle_receive_proc p send_map channels_ctxt fun_name in
              (was_opt, Spawn (ch, exp, st, new_proc, v_list)))
          | Some multisends ->
              (* The multisend of a function applied must be {InBody} *)
              let extern =
                List.filter
                  (fun x -> match x with External _ -> true | _ -> false)
                  multisends
              in
              (* Add channel with matching multisend to the context *)
              let new_ctxt =
                if List.length extern > 0 then StringMap.add ch extern channels_ctxt
                else channels_ctxt
              in
              let was_opt, new_proc = bundle_receive_proc p send_map new_ctxt fun_name in
              (was_opt, Spawn (ch, exp, st, new_proc, v_list)))
      | _ -> (
          match StringMap.find_opt fun_name send_map with
          | Some multisends ->
              (* The multisends found must be {InSpawn} *)
              (* Add channel with matching multisend to the context *)
              let internal =
                List.filter
                  (fun x -> match x with Internal _ -> true | _ -> false)
                  multisends
              in
              let new_ctxt =
                if List.length internal > 0 then StringMap.add ch internal channels_ctxt
                else channels_ctxt
              in
              let was_opt, exp1 = bundle_receive_exp exp send_map new_ctxt fun_name in
              let was_opt1, new_proc = bundle_receive_proc p send_map new_ctxt fun_name in
              (was_opt || was_opt1, Spawn (ch, exp1, st, new_proc, v_list))
          | None ->
              (*No matching multisend found*)
              (* The context is new inside the spawn, no need to send the channels in current context *)
              let was_opt1, exp1 =
                bundle_receive_exp exp send_map channels_ctxt fun_name
              in
              let was_opt2, new_proc =
                bundle_receive_proc p send_map channels_ctxt fun_name
              in
              (was_opt1 || was_opt2, Spawn (ch, exp1, st, new_proc, v_list))))
  | TailSpawn (v, exp, st, args, b, arg, l) ->
      (* TailSpawn don't have any Multireceive following  *)
      let was_opt, exp1 = bundle_receive_exp exp send_map channels_ctxt fun_name in
      (was_opt, TailSpawn (v, exp1, st, args, b, arg, l))
  | Choice (ch, list) ->
      let was_opt, new_list =
        bundle_receive_choice_list list send_map channels_ctxt fun_name
      in
      (was_opt, Choice (ch, new_list))
  | Label (v1, v2, proc, st) ->
      let was_opt, new_proc = bundle_receive_proc proc send_map channels_ctxt fun_name in
      (was_opt, Label (v1, v2, new_proc, st))
  | SendChan (v1, v2, st, proc) ->
      let was_opt, new_proc = bundle_receive_proc proc send_map channels_ctxt fun_name in
      (was_opt, SendChan (v1, v2, st, new_proc))
  | RecvChan (v1, v2, st, proc) ->
      let was_opt, new_proc = bundle_receive_proc proc send_map channels_ctxt fun_name in
      (was_opt, RecvChan (v1, v2, st, new_proc))
  | Print (exp, proc) ->
      let was_opt, new_proc = bundle_receive_proc proc send_map channels_ctxt fun_name in
      (was_opt, Print (exp, new_proc))
  | If (exp, proc1, proc2) ->
      let op1, new_exp1 = bundle_receive_exp exp send_map channels_ctxt fun_name in
      let op2, new_proc1 = bundle_receive_proc proc1 send_map channels_ctxt fun_name in
      let op3, new_proc2 = bundle_receive_proc proc2 send_map channels_ctxt fun_name in
      (op1 || op2 || op3, If (new_exp1, new_proc1, new_proc2))
  | MultiSend (ch, to_send_exps, types, next) ->
      let was_opt, new_proc = bundle_receive_proc next send_map channels_ctxt fun_name in
      (was_opt, MultiSend (ch, to_send_exps, types, new_proc))
  | _ -> (false, proc)
(*Multireceive don't occur*)

and bundle_receive_choice_list list send_map channels_ctxt fun_name =
  match list with
  | (v, (proc, st)) :: tail ->
      let was_opt, new_proc = bundle_receive_proc proc send_map channels_ctxt fun_name in
      let was_opt2, new_list =
        bundle_receive_choice_list tail send_map channels_ctxt fun_name
      in
      (was_opt || was_opt2, (v, (new_proc, st)) :: new_list)
  | [] -> (false, [])
