open Syntax
open Typechecker
open Printer
open Buffer

let logger = Logs.Src.create "Sessint"

(** Signals if the print import is needed  *)
let needs_print = ref false

let compile_with_struct = ref true

(** Reference to a boolean to indicate the compilation strategy: 
    {!true} uses a single channel to represent a session type
    {!false} uses multiple channels to represent a session type.contents.
    This reference is set in {!compile_prog} and it is used in several functions *)
let single_channel = ref true

(** Buffer to write the preamble phase of the compilation. It is a global reference that is assessed by several functions, this avoids passing the buffer as argument in many functions*)
let compiled = ref (Buffer.create 5000)

type state_body =
  | Comm of ty * state ref
      (** representes a send/receive session type and contains the type of value exchanged int the communication, and a reference to the next state*)
  | MultiComm of ty list * state ref
      (** representes a multi send/receive session type and contains the type of value exchanged int the communication, and a reference to the next state*)
  | Choice of (string * state ref) list  (**Represents a choice session type*)
  | End  (**represents the termination session type*)

(** A {!state} is a tuple containing the state's name/identifiers, and the body of the state itself  *)
and state = State of string * state_body

(* Id generation *)

module SeqMap = Map.Make (String)
(** Since go doesn't allow for the redefinition of ids as another type, we must generate 'intermediate' ids for each state. *)

let curr_seq_id id map =
  match SeqMap.find_opt id map with
  | Some num_ref -> id ^ string_of_int !num_ref
  | None -> id

let next_seq_id id map =
  match SeqMap.find_opt id map with
  | None ->
      let num_ref = ref 0 in
      let n_map = SeqMap.add id num_ref map in
      (curr_seq_id id n_map, n_map)
  | Some num_ref ->
      incr num_ref;
      (curr_seq_id id map, map)

let copy_id_map map =
  let n_map = ref SeqMap.empty in
  SeqMap.iter
    (fun k v ->
      let _ = n_map := SeqMap.add k (ref !v) !n_map in
      ())
    map;
  !n_map

(** Map that contains stype->state associations to keep track of 
    
Used to maintain a map of stypes and their respective states *)
module StypeKey = struct
  type t = stype

  let hash _ = 1
  let equal a b = subtyping [] a b || subtyping [] b a
end

module TypeTable = Hashtbl.Make (StypeKey)

(** Module that keeps stype->id associations to keep track of state names*)
module TypeStore : sig
  val get_id : stype -> string
end = struct
  let make_fresh_id =
    let id_curr = ref (-1) in
    fun () ->
      incr id_curr;
      "_state_" ^ string_of_int !id_curr

  let hash_table =
    let init_table = ref (TypeTable.create 50) in
    fun () -> !init_table

  (** Identifier generation is done statically; a single integer reference is kept, from which all identifiers (which are a concatenation of a string and the former integer) draw from, incrementing the referred integer when calling for a new identifier *)
  let get_id stype =
    let table = hash_table () in
    match TypeTable.find_opt table stype with
    | Some id -> id
    | None ->
        let new_id = make_fresh_id () in
        TypeTable.replace table stype new_id;
        new_id
end

(** Make a state from a given stype 
The function has a small performance optimization: is the received session type already existes in a pair in the type-state map, the map is returned immediately
  @param map the stype->state map, a map if session type-state pairs
  @param env: an environment containing previously found recursion variables, and references that will eventually lead to the state that matches the start of the respective recursion
  @param stype the session type to build a state from
  @return the passed session type-state map, enriched with the associations related to the received type and the newly generated state, as well as any associations related to the underlying session types.
  
  *)
let rec make_state map env stype =
  match TypeTable.find_opt map stype with
  | Some _ -> map
  | None -> (
      match stype with
      | STSend (t, st) -> (
          (* The function first generates a new identifier for the soon to be created state *)
          let state_id = TypeStore.get_id stype in
          match st with
          | STRec (v, _) -> (
              (* If the continuing session type is the recursive type, the function checks for the existence of the recursion variable in the environment: if it does not  exist, that means the current type is not inside a recursion with that specific recursion variable, and the function proceeds as before *)
              match List.assoc_opt v env with
              | Some dummy_ref ->
                  (* if it does exist, the function will create the state matching the current session type, with a reference that points to a dummy state, which will, once the current execution returns, be changed to point to the start of the recursion *)
                  let this_state = State (state_id, Comm (t, dummy_ref)) in
                  TypeTable.replace map stype this_state;
                  map
              | None ->
                  let new_map = make_state map env st in
                  let this_state =
                    State (state_id, Comm (t, ref (TypeTable.find new_map st)))
                  in
                  TypeTable.replace new_map stype this_state;
                  new_map)
          | _ ->
              (*if the continuing session type is not the recursive type, the function proceeds“straightforwardly”, calls itself for the continuing state, creates the new state proper, and modifies and returns the association map *)
              let new_map = make_state map env st in
              let this_state =
                State (state_id, Comm (t, ref (TypeTable.find new_map st)))
              in
              TypeTable.replace new_map stype this_state;
              new_map)
      | STMultiSend (types, st) -> (
          let state_id = TypeStore.get_id stype in
          match st with
          | STRec (v, _) -> (
              match List.assoc_opt v env with
              | Some dummy_ref ->
                  let this_state = State (state_id, MultiComm (types, dummy_ref)) in
                  TypeTable.replace map stype this_state;
                  map
              | None ->
                  let new_map = make_state map env st in
                  let this_state =
                    State (state_id, MultiComm (types, ref (TypeTable.find new_map st)))
                  in
                  TypeTable.replace new_map stype this_state;
                  new_map)
          | _ ->
              let new_map = make_state map env st in
              let this_state =
                State (state_id, MultiComm (types, ref (TypeTable.find new_map st)))
              in
              TypeTable.replace new_map stype this_state;
              new_map)
      | STRecv (t, st) -> (
          let state_id = TypeStore.get_id stype in
          match st with
          | STRec (v, _) -> (
              match List.assoc_opt v env with
              | Some dummy_ref ->
                  let this_state = State (state_id, Comm (t, dummy_ref)) in
                  TypeTable.replace map stype this_state;
                  map
              | None ->
                  let new_map = make_state map env st in
                  let this_state =
                    State (state_id, Comm (t, ref (TypeTable.find new_map st)))
                  in
                  TypeTable.replace new_map stype this_state;
                  new_map)
          | _ ->
              let new_map = make_state map env st in
              let this_state =
                State (state_id, Comm (t, ref (TypeTable.find new_map st)))
              in
              TypeTable.replace new_map stype this_state;
              new_map)
      | STMultiRecv (types, st) -> (
          let state_id = TypeStore.get_id stype in
          match st with
          | STRec (v, _) -> (
              match List.assoc_opt v env with
              | Some dummy_ref ->
                  let this_state = State (state_id, MultiComm (types, dummy_ref)) in
                  TypeTable.replace map stype this_state;
                  map
              | None ->
                  let new_map = make_state map env st in
                  let this_state =
                    State (state_id, MultiComm (types, ref (TypeTable.find new_map st)))
                  in
                  TypeTable.replace new_map stype this_state;
                  new_map)
          | _ ->
              let new_map = make_state map env st in
              let this_state =
                State (state_id, MultiComm (types, ref (TypeTable.find new_map st)))
              in
              TypeTable.replace new_map stype this_state;
              new_map)
      | STEnd ->
          let state_id = TypeStore.get_id stype in
          TypeTable.replace map stype (State (state_id, End));
          map
      | STExtChoice l ->
          let state_id = TypeStore.get_id stype in
          let n_map, state_pair_list = make_state_list map env l in
          let state = State (state_id, Choice state_pair_list) in
          TypeTable.replace n_map stype state;
          n_map
      | STIntChoice l ->
          let state_id = TypeStore.get_id stype in
          let n_map, state_pair_list = make_state_list map env l in
          let state = State (state_id, Choice state_pair_list) in
          TypeTable.replace n_map stype state;
          n_map
      | STSendChan (st1, st2) -> (
          let state_id = TypeStore.get_id stype in
          match st2 with
          | STRec (v, _) -> (
              match List.assoc_opt v env with
              | Some dummy_ref ->
                  let this_state = State (state_id, Comm (TProc (st1, []), dummy_ref)) in
                  TypeTable.replace map stype this_state;
                  map
              | None ->
                  let new_map = make_state map env st2 in
                  let this_state =
                    State
                      (state_id, Comm (TProc (st1, []), ref (TypeTable.find new_map st2)))
                  in
                  TypeTable.replace new_map stype this_state;
                  new_map)
          | _ ->
              let new_map = make_state map env st2 in
              let this_state =
                State (state_id, Comm (TProc (st1, []), ref (TypeTable.find new_map st2)))
              in
              TypeTable.replace new_map stype this_state;
              new_map)
      | STRecvChan (st1, st2) -> (
          let state_id = TypeStore.get_id stype in
          match st2 with
          | STRec (v, _) -> (
              match List.assoc_opt v env with
              | Some dummy_ref ->
                  let this_state = State (state_id, Comm (TProc (st1, []), dummy_ref)) in
                  TypeTable.replace map stype this_state;
                  map
              | None ->
                  let new_map = make_state map env st2 in
                  let this_state =
                    State
                      (state_id, Comm (TProc (st1, []), ref (TypeTable.find new_map st2)))
                  in
                  TypeTable.replace new_map stype this_state;
                  new_map)
          | _ ->
              let new_map = make_state map env st2 in
              let this_state =
                State (state_id, Comm (TProc (st1, []), ref (TypeTable.find new_map st2)))
              in
              TypeTable.replace new_map stype this_state;
              new_map)
      | STVar _ -> assert false (* this case is never reached *)
      | STRec (v, st) -> (
          (* Special attention should be payed to the case of creating a state from a recursive  type, which involves an indirect reference to its next state *)
          match List.assoc_opt v env with
          (* If the session type is a recursive type, yhe function checks {env} for the existence of recursion variable {v}. If it exists, then the recursive type has already been encountered and already exists in the association map which is promptly returned *)
          | Some _ -> map
          (* If it does not exist, then the function will first create {n_state}, a dummy state, which will not be presented in the final state map *)
          | None ->
              let n_state = State ("%", End) in
              (* Next a reference is created to such dummy state, this will be the reference that loops around states in the recursion, and starts the cycle again *)
              let start_ref = ref n_state in
              (* The next step is adding to the environment a pair containing the recursion variable of the recursive type, and the newly created reference; this pair will signal to the final state pre-recursion that its reference must be treated in a specific way *)
              let nenv = (v, start_ref) :: env in
              (* Afterwards, there is a substitution of the recursion variable by the recursive type in the continuing session type, resulting in {unfolded_st}, ensuring the {make_state} function will keep working with closed recursive types. *)
              let unfolded_st = subst_stype stype v st in
              (* Then the function itself is called again for {unfolded_st} *)
              let new_map = make_state map nenv unfolded_st in
              (* and the reference created at the start is changed to point to the state matching {unfolded_st} thus closing the cycle *)
              start_ref := TypeTable.find new_map unfolded_st;
              (* The association map is then altered and returned *)
              TypeTable.replace new_map stype !start_ref;
              new_map)
      | STUVar _ -> assert false)

and make_state_list map env list =
  let fold_fun (curr_map, pairs) (l, sty) =
    match sty with
    | STRec (v, _) -> (
        match List.assoc_opt v env with
        | Some dummy_ref -> (curr_map, (l, dummy_ref) :: pairs)
        | None ->
            let n_map = make_state curr_map env sty in
            (n_map, (l, ref (TypeTable.find n_map sty)) :: pairs))
    | _ ->
        let n_map = make_state curr_map env sty in
        (n_map, (l, ref (TypeTable.find n_map sty)) :: pairs)
  in
  List.fold_left fold_fun (map, []) list

(** Generates the map of type-state associations. There is an exploration  of the abstract syntax trees of both declarations and the main expression of the program, searching for process expressions, and retrieving their session types (which we put in place during typechecking). For each session type that is found, there is a call to {!make_state} which modifies the map associations
@param map a map containing associations of session types and states
@param e the expression to be traversed *)
let rec make_state_trees_from_exp map e =
  match e with
  | UnitVal | Num _ | Bool _ -> map
  | FunDef (_, _, e', _) -> make_state_trees_from_exp map e'
  | RecFunDef (_, _, e', _) -> make_state_trees_from_exp map e'
  | Var _ -> map
  | BOp (_, e1, e2) ->
      let fst_map = make_state_trees_from_exp map e1 in
      make_state_trees_from_exp fst_map e2
  | UOp (_, e') -> make_state_trees_from_exp map e'
  | Let (_, e1, e2) ->
      let fst_map = make_state_trees_from_exp map e1 in
      make_state_trees_from_exp fst_map e2
  | FunApp (e1, e2) ->
      let fst_map = make_state_trees_from_exp map e1 in
      make_state_trees_from_exp fst_map e2
  | Annot (e', _) -> make_state_trees_from_exp map e'
  | Cond (cond, e1, e2) ->
      let fst_map = make_state_trees_from_exp map cond in
      let snd_map = make_state_trees_from_exp fst_map e1 in
      make_state_trees_from_exp snd_map e2
  | ProcExp (_, p, opt, _) -> (
      match opt with
      | Some st ->
          let fst_map = make_state map [] st in
          make_state_trees_from_proc fst_map p
      | None -> assert false)
  | ExecExp exp -> make_state_trees_from_exp map exp

and make_state_trees_from_multi map exps =
  match exps with
  | [] -> map
  | exp :: rest ->
      let fst_map = make_state_trees_from_exp map exp in
      make_state_trees_from_multi fst_map rest

and make_state_trees_from_proc map p =
  match p with
  | Send (_, exp, _, proc) ->
      let fst_map = make_state_trees_from_exp map exp in
      make_state_trees_from_proc fst_map proc
  | MultiSend (_, to_send, _, next) ->
      let fst_map = make_state_trees_from_multi map to_send in
      make_state_trees_from_proc fst_map next
  | Recv (_, _, _, proc) -> make_state_trees_from_proc map proc
  | MultiRecv (_, _, _, next) -> make_state_trees_from_proc map next
  | Close _ -> map
  | Wait (_, proc) -> make_state_trees_from_proc map proc
  | Fwd (_, _, _) -> map
  | Spawn (_, exp, _, proc, _) ->
      let fst_map = make_state_trees_from_exp map exp in
      make_state_trees_from_proc fst_map proc
  | TailSpawn (_, exp, _, _, _, _, _) -> make_state_trees_from_exp map exp
  | Choice (_, l) ->
      let fold_fun curr_map (_, (proc, _)) = make_state_trees_from_proc curr_map proc in
      List.fold_left fold_fun map l
  | Label (_, _, proc, _) -> make_state_trees_from_proc map proc
  | SendChan (_, _, _, proc) -> make_state_trees_from_proc map proc
  | RecvChan (_, _, _, proc) -> make_state_trees_from_proc map proc
  | Print (exp, proc) ->
      let fst_map = make_state_trees_from_exp map exp in
      make_state_trees_from_proc fst_map proc
  | If (e, p1, p2) ->
      let fst_map = make_state_trees_from_exp map e in
      let snd_map = make_state_trees_from_proc fst_map p1 in
      make_state_trees_from_proc snd_map p2

(* Auxiliary boilerplate compile functions *)

(** Multichannel boilerplate *)
let make_from_comm_template_multi_channel name ty next =
  Printf.bprintf !compiled "type %s struct {\n" name;
  Printf.bprintf !compiled "    c chan %s\n" ty;
  Printf.bprintf !compiled "    next *%s\n" next;
  Printf.bprintf !compiled "    mtx sync.Mutex\n";
  Printf.bprintf !compiled "}\n";
  Printf.bprintf !compiled "func init%s() *%s {\n" name name;
  Printf.bprintf !compiled "    return &%s{ make(chan %s), nil, sync.Mutex{} }\n" name ty;
  Printf.bprintf !compiled "}\n";
  Printf.bprintf !compiled "func (x *%s) Send(v %s) *%s {\n" name ty next;
  Printf.bprintf !compiled "    x.mtx.Lock();\n";
  Printf.bprintf !compiled "    if x.next == nil { x.next = init%s() }\n" next;
  Printf.bprintf !compiled "    x.mtx.Unlock();\n";
  Printf.bprintf !compiled "    x.c <- v;\n";
  Printf.bprintf !compiled "    return %s\n" (if name != next then "x.next" else "x");
  Printf.bprintf !compiled "}\n";
  Printf.bprintf !compiled "func (x *%s) Recv() (%s, *%s) {\n" name ty next;
  Printf.bprintf !compiled "    x.mtx.Lock();\n";
  Printf.bprintf !compiled "    if x.next == nil { x.next = init%s() }\n" next;
  Printf.bprintf !compiled "    x.mtx.Unlock();\n";
  Printf.bprintf !compiled "    return <-x.c, %s\n"
    (if name != next then "x.next" else "x");
  Printf.bprintf !compiled "}\n\n "

let make_from_end_template_multi_channel name =
  Printf.bprintf !compiled "type %s struct {\n    c chan interface{}\n}\n" name;
  Printf.bprintf !compiled "func init%s() *%s { return &%s{ make(chan interface{}) } }\n"
    name name name;
  Printf.bprintf !compiled "func (x *%s) Send(v interface{}) { x.c <- v }\n" name;
  Printf.bprintf !compiled "func (x *%s) Recv() interface{} { return <-x.c }\n\n  " name

let rec make_from_label_pairs_multi_channel label_pairs =
  match label_pairs with
  | (l, rf) :: tail -> (
      match !rf with
      | State (next_id, _) ->
          Printf.bprintf !compiled "\tm[\"%s\"] = init%s()\n" l next_id;
          make_from_label_pairs_multi_channel tail)
  | [] -> ()

let make_from_choice_template_multi_channel name label_pairs =
  Printf.bprintf !compiled "type %s struct {\n" name;
  Printf.bprintf !compiled "    c chan string\n";
  Printf.bprintf !compiled "    ls map[string]interface{}\n";
  Printf.bprintf !compiled "}\n";
  Printf.bprintf !compiled "func init%s() *%s {\n" name name;
  Printf.bprintf !compiled "    m := make(map[string]interface{})\n";
  make_from_label_pairs_multi_channel label_pairs;
  Printf.bprintf !compiled "    return &%s{make(chan string), m} }\n" name;
  Printf.bprintf !compiled "func (x *%s) Send(v string) { x.c <- v }\n" name;
  Printf.bprintf !compiled "func (x *%s) Recv() string  { return <-x.c }\n\n  " name

(* Single channel boilerplate *)
let make_from_comm_template_single_channel name ty next =
  Printf.bprintf !compiled "type %s struct {\n" name;
  Printf.bprintf !compiled "   c chan interface{}\n";
  Printf.bprintf !compiled "   next *%s\n}\n\n" next;
  Printf.bprintf !compiled "func init%s(c chan interface{}) *%s { return &%s{c, nil} }\n"
    name name name;
  Printf.bprintf !compiled "func (x *%s) Send(v %s) *%s {\n" name ty next;
  Printf.bprintf !compiled
    "   if x.next == nil { x.next = init%s(x.c) }; x.c <- v; return %s }\n" next
    (if name != next then "x.next" else "x");
  Printf.bprintf !compiled "func (x *%s) Recv() (%s, *%s) {\n" name ty next;
  Printf.bprintf !compiled
    "   if x.next == nil { x.next = init%s(x.c) }; return (<-x.c).(%s), %s }\n\n  " next
    ty
    (if name != next then "x.next" else "x")

(** To resolve the name of the type  
    @param type_map type-state map 
    @param ty the type to actually compile 
    @return a string version of the type in Go code. *)
let rec compile_type_string type_map ty =
  match ty with
  | TUnit -> "interface{}"
  | TNum -> "int"
  | TBool -> "bool"
  | TFun (t1, t2) -> (
      match t1 with
      | TUnit -> Printf.sprintf "func () %s" (compile_type_string type_map t2)
      | _ ->
          Printf.sprintf "func (_x %s) %s"
            (compile_type_string type_map t1)
            (compile_type_string type_map t2))
  | TProc (st, ctxt) -> (
      match TypeTable.find_opt type_map st with
      | Some (State (id, _)) ->
          if List.length ctxt = 0 then Printf.sprintf "func (_x *%s)" id
          else
            Printf.sprintf "func (_x *%s, %s)" id
              (compile_lin_ctxt_to_fun_args_string ctxt type_map)
      | None -> assert false)
  | TVar v -> v
(* This case is never actually reached if, as expected, all TVars have previously been expanded into their primitive equivalents *)

and compile_lin_ctxt_to_fun_args_string ctxt type_map =
  match ctxt with
  | [ (c, st) ] -> (
      match TypeTable.find_opt type_map st with
      | Some (State (id, _)) -> Printf.sprintf "%s *%s" c id
      | None -> assert false)
  | (c, st) :: tail -> (
      match TypeTable.find_opt type_map st with
      | Some (State (id, _)) ->
          Printf.sprintf "%s *%s, %s" c id
            (compile_lin_ctxt_to_fun_args_string tail type_map)
      | None -> assert false)
  | [] -> ""

(** Compiles the parameters of a Send function of a MultiSend - parameter name and types*)
let rec make_args_list_types types type_map =
  match types with
  | [ (t, i) ] -> Printf.bprintf !compiled "v%d %s" i (compile_type_string type_map t)
  | (t, i) :: tail ->
      Printf.bprintf !compiled "v%d %s, " i (compile_type_string type_map t);
      make_args_list_types tail type_map
  | [] -> ()

(**Compiles the variables of a Receive function associated with a Multireceive*)
let rec make_return_types_list_types types type_map =
  match types with
  | [ (t, _) ] -> Printf.bprintf !compiled "%s" (compile_type_string type_map t)
  | (t, _) :: tail ->
      Printf.bprintf !compiled "%s, " (compile_type_string type_map t);
      make_return_types_list_types tail type_map
  | [] -> ()

(** Compiles the return values associated to a MultiReceive - a cast is necessary *)
let make_return_recv_list types type_map =
  Printf.bprintf !compiled "ll := <- x.c\n";
  Printf.bprintf !compiled "l := ll.([]interface{})\n";
  Printf.bprintf !compiled "return ";
  let rec make_return types =
    match types with
    | [ (t, i) ] ->
        Printf.bprintf !compiled "l[%d].(%s)" i (compile_type_string type_map t)
    | (t, i) :: tail ->
        Printf.bprintf !compiled "l[%d].(%s), " i (compile_type_string type_map t);
        make_return tail
    | [] -> ()
  in
  make_return types

(** Compiles a list of variables to send on a list of a MultiSend *)
let rec make_list_send types =
  match types with
  | [ (_, i) ] -> Printf.bprintf !compiled "v%d" i
  | (_, i) :: tail ->
      Printf.bprintf !compiled "v%d, " i;
      make_list_send tail
  | [] -> Printf.bprintf !compiled "}\n"

let rec make_field_struct_multisend types type_map =
  match types with
  | (t, i) :: tail ->
      Printf.bprintf !compiled "v%d %s\n" i (compile_type_string type_map t);
      make_field_struct_multisend tail type_map
  | [] -> ()

let make_return_recv_struct types struct_name =
  Printf.bprintf !compiled "ll := <- x.c\n";
  Printf.bprintf !compiled "l := ll.(%s)\n" struct_name;
  Printf.bprintf !compiled "return ";
  let rec make_return types =
    match types with
    | [ (_, i) ] -> Printf.bprintf !compiled "l.v%d" i
    | (_, i) :: tail ->
        Printf.bprintf !compiled "l.v%d, " i;
        make_return tail
    | [] -> ()
  in
  make_return types

let compile_multi_with_struct name types_i next type_map =
  let struct_name = "_multisend_type_" ^ name in
  Printf.bprintf !compiled "type %s struct {\n" struct_name;
  make_field_struct_multisend types_i type_map;
  Buffer.add_string !compiled "}\n";
  Printf.bprintf !compiled "func (x *%s) Send(" name;
  make_args_list_types types_i type_map;
  Printf.bprintf !compiled ") *%s {\n" next;
  Printf.bprintf !compiled "   if x.next == nil { x.next = init%s(x.c) };\n" next;
  Printf.bprintf !compiled " x.c <- %s{" struct_name;
  make_list_send types_i;
  Buffer.add_string !compiled "}\n";
  Printf.bprintf !compiled "return %s }\n" (if name != next then "x.next" else "x");
  Printf.bprintf !compiled "func (x *%s) Recv() (" name;
  make_return_types_list_types types_i type_map;
  Printf.bprintf !compiled ", *%s) {\n" next;
  Printf.bprintf !compiled "   if x.next == nil { x.next = init%s(x.c) };" next;
  make_return_recv_struct types_i struct_name;
  Printf.bprintf !compiled ", %s }\n\n" (if name != next then "x.next" else "x")

let compile_multi_methods_list name types_i next type_map =
  Printf.bprintf !compiled "func (x *%s) Send(" name;
  make_args_list_types types_i type_map;
  Printf.bprintf !compiled ") *%s {\n" next;
  Printf.bprintf !compiled "   if x.next == nil { x.next = init%s(x.c) };\n" next;
  Printf.bprintf !compiled " x.c <- []interface{}{";
  make_list_send types_i;
  Buffer.add_string !compiled "}\n";
  Printf.bprintf !compiled "return %s }\n" (if name != next then "x.next" else "x");
  Printf.bprintf !compiled "func (x *%s) Recv() (" name;
  make_return_types_list_types types_i type_map;
  Printf.bprintf !compiled ", *%s) {\n" next;
  Printf.bprintf !compiled "   if x.next == nil { x.next = init%s(x.c) };" next;
  make_return_recv_list types_i type_map;
  Printf.bprintf !compiled ", %s }\n\n" (if name != next then "x.next" else "x")

let make_from_multi_comm_template_single_channel name types next type_map =
  let types_i = List.mapi (fun i t -> (t, i)) types in
  Printf.bprintf !compiled "type %s struct {\n" name;
  Printf.bprintf !compiled "   c chan interface{}\n";
  Printf.bprintf !compiled "   next *%s\n}\n\n" next;
  Printf.bprintf !compiled "func init%s(c chan interface{}) *%s { return &%s{c, nil} }\n"
    name name name;
  if !compile_with_struct then compile_multi_with_struct name types_i next type_map
  else compile_multi_methods_list name types_i next type_map

let make_from_end_template_single_channel name =
  Printf.bprintf !compiled "type %s struct {\n" name;
  Printf.bprintf !compiled "    c chan interface{}\n}\n";
  Printf.bprintf !compiled "func init%s(c chan interface{}) *%s { return &%s{ c } }\n"
    name name name;
  Printf.bprintf !compiled "func (x *%s) Send(v interface{}) { x.c <- v }\n" name;
  Printf.bprintf !compiled "func (x *%s) Recv() interface{} { return <-x.c }\n\n  " name

let rec make_from_label_pairs_single_channel label_pairs name =
  match label_pairs with
  | (l, rf) :: tail -> (
      match !rf with
      | State (next_id, _) ->
          if next_id = name then
            (* To compile recursive types *)
            Printf.bprintf !compiled "\tm[\"%s\"] = &%s{ c, m }\n" l name
          else Printf.bprintf !compiled "\tm[\"%s\"] = init%s( c )\n" l next_id;
          make_from_label_pairs_single_channel tail name)
  | [] -> ()

let make_from_choice_template_single_channel name label_pairs =
  Printf.bprintf !compiled "type %s struct {\n" name;
  Buffer.add_string !compiled "    c  chan interface{}\n";
  Buffer.add_string !compiled "    ls map[string]interface{}\n  }\n  ";
  Printf.bprintf !compiled
    "func init%s(c chan interface{}) *%s { m := make(map[string]interface{})\n " name name;
  make_from_label_pairs_single_channel label_pairs name;
  Printf.bprintf !compiled "   return &%s{ c, m } }\n" name;
  Printf.bprintf !compiled "func (x *%s) Send(v string) { x.c <- v }\n" name;
  Printf.bprintf !compiled "func (x *%s) Recv() string  { return (<-x.c).(string) }\n\n  "
    name

(* Preamble compilation functions in earnest *)

(** To resolve the name of the type  
    @param type_map type-state map 
    @param ty the type to actually compile 
    @return a string version of the type in Go code. *)
let rec compile_type type_map ty : unit =
  match ty with
  | TUnit -> Buffer.add_string !compiled "interface{}"
  | TNum -> Buffer.add_string !compiled "int"
  | TBool -> Buffer.add_string !compiled "bool"
  | TFun (t1, t2) -> (
      match t1 with
      | TUnit ->
          Buffer.add_string !compiled "func () ";
          compile_type type_map t2
      | _ ->
          Buffer.add_string !compiled "func (_x ";
          compile_type type_map t1;
          Buffer.add_string !compiled ") ";
          compile_type type_map t2)
  | TProc (st, ctxt) -> (
      match TypeTable.find_opt type_map st with
      | Some (State (id, _)) ->
          if List.length ctxt = 0 then Printf.bprintf !compiled "func (_x *%s)" id
          else (
            Printf.bprintf !compiled "func (_x *%s, " id;
            compile_lin_ctxt_to_fun_args ctxt type_map;
            Buffer.add_string !compiled ")")
      | None -> assert false)
  | TVar v -> Buffer.add_string !compiled v
(* This case is never actually reached if, as expected, all TVars have previously been expanded into their primitive equivalents *)

and compile_lin_ctxt_to_fun_args ctxt type_map =
  (* NEW correção para troca a ordem dos argumentos *)
  (* We need to reverse the list  otherwise the channels used appear on the wrong order*)
  let new_ctxt = List.rev ctxt in
  match new_ctxt with
  | [ (c, st) ] -> (
      match TypeTable.find_opt type_map st with
      | Some (State (id, _)) -> Printf.bprintf !compiled "%s *%s" c id
      | None -> assert false)
  | (c, st) :: tail -> (
      match TypeTable.find_opt type_map st with
      | Some (State (id, _)) ->
          Printf.bprintf !compiled "%s *%s, " c id;
          compile_lin_ctxt_to_fun_args tail type_map
      | None -> assert false)
  | [] -> ()

(** The compile_statefun may be the one that compiles with for a single channel
   or the one that compiles for multichannel.*)
let rec compile_from_state_list state_list type_map compile_statefun =
  (* let str = ref "" in Seq.iter (fun state -> str := !str ^ (compile_statefun type_map
      state)) state_list; !str *)
  match state_list with
  | state :: tail ->
      compile_statefun type_map state;
      compile_from_state_list tail type_map compile_statefun
  | [] -> ()

(** Multichannel compilation *)
let compile_state_multi_channel type_map state =
  match state with
  | State (id, state_body) -> (
      match state_body with
      | Comm (t, r) -> (
          match !r with
          | State (next_id, _) -> (
              match t with
              | TProc (st, _) ->
                  make_from_comm_template_multi_channel id
                    ("*" ^ TypeStore.get_id st)
                    next_id
              | _ ->
                  make_from_comm_template_multi_channel id
                    (compile_type_string type_map t)
                    next_id))
      | Choice l -> make_from_choice_template_multi_channel id l
      | End -> make_from_end_template_multi_channel id
      | MultiComm (_types, _st) ->
          assert false (*optimizations only occur in single channel mode*))

(** Single channel compilation *)
let compile_state_single_channel type_map state =
  match state with
  | State (id, state_body) -> (
      match state_body with
      | Comm (t, r) -> (
          match !r with
          | State (next_id, _) -> (
              match t with
              | TProc (_, _) ->
                  make_from_comm_template_single_channel id "interface{}" next_id
              | _ ->
                  make_from_comm_template_single_channel id
                    (compile_type_string type_map t)
                    next_id))
      | Choice l -> make_from_choice_template_single_channel id l
      | End -> make_from_end_template_single_channel id
      | MultiComm (types, st) -> (
          match !st with
          | State (next_id, _) ->
              make_from_multi_comm_template_single_channel id types next_id type_map))

(* Instructions compilation *)

(** Updates the arguments in a recursive function call
    @param spawn_args additional channels used in the spawn that launches a recursive call, to compute the new value
    @param original_names names used as the function parameters, to update the values 
    @param id_map used to determine what is the updated value for the channel*)
let rec compile_list_spawn_args spawn_args original_names id_map =
  match (spawn_args, original_names) with
  | update :: update_tail, name :: original_tail ->
      Printf.bprintf !compiled "%s = %s\n " name (curr_seq_id update id_map);
      compile_list_spawn_args update_tail original_tail id_map
  | _ -> ()

(** The single_channel parameter indicates whether to compile for single channel or multi channel 
@param type_map the type-state map
@param exp the expression to compile
@param functional environment of identifiers and respective functional values, which may needed to evaluate and simplify a function application before compile it
*)
let rec compile_exp type_map exp env is_rec =
  match exp with
  | UnitVal -> Buffer.add_string !compiled "struct{}{}"
  | Num n -> Buffer.add_string !compiled (string_of_int n)
  | Bool b -> Buffer.add_string !compiled (string_of_bool b)
  | FunDef (v, ty, e', ret) -> (
      match (ty, ret) with
      | Some t1, Some t2 -> (
          match t1 with
          | TUnit ->
              Buffer.add_string !compiled "(func () ";
              compile_type type_map t2;
              Buffer.add_string !compiled " {";
              compile_exp type_map e' env is_rec;
              Buffer.add_string !compiled "})"
          | _ ->
              Printf.bprintf !compiled "(func (%s " v;
              compile_type type_map t1;
              Buffer.add_string !compiled ") ";
              compile_type type_map t2;
              Buffer.add_string !compiled " {";
              compile_exp type_map e' env is_rec;
              Buffer.add_string !compiled "})")
      | _, _ -> assert false)
  | RecFunDef (_, _, _, _) -> assert false
  | Var v -> Buffer.add_string !compiled v
  | BOp (_, _, _) -> compile_bop type_map exp env is_rec
  | UOp (_, _) -> compile_uop type_map exp env is_rec
  (*Let expressions must be fully interpreted before being compiled, otherwise there is no way  to simply define x in Go (x:=e1) and keep the let as a possible expression, usable anywhere in the code*)
  | Let (x, e1, e2) ->
      let nenv = Interpreter.StrMap.add x e1 env in
      let ne2 = Interpreter.eval nenv e2 in
      compile_exp type_map ne2 nenv is_rec
      (* x ^ " := " ^ compile_exp type_map e1 env ^ "\n" ^ compile_exp type_map e2 env *)
  | FunApp (e1, e2) -> (
      match e2 with
      | UnitVal ->
          compile_exp type_map e1 env is_rec;
          Buffer.add_string !compiled "()"
      | _ ->
          compile_exp type_map e1 env is_rec;
          Buffer.add_string !compiled "(";
          compile_exp type_map e2 env is_rec;
          Buffer.add_string !compiled ")")
  | Annot (e', _) -> compile_exp type_map e' env is_rec
  | Cond (cond, e1, e2) ->
      Buffer.add_string !compiled "if ";
      compile_exp type_map cond env is_rec;
      Buffer.add_string !compiled " {\n";
      compile_exp type_map e1 env is_rec;
      Buffer.add_string !compiled "\n} else {\n";
      compile_exp type_map e2 env is_rec;
      Buffer.add_string !compiled "\n}\n"
  | ProcExp (c, p, opt, ctxt) -> (
      (* c -> name of the channel
         p -> process
         opt -> type of the process
         ctxt -> things that the process uses *)
      match opt with
      | Some st -> (
          (* The map's key is the type itself and the values is a string whit the correspondent Go type *)
          match TypeTable.find_opt type_map st with
          | Some (State (id, _)) ->
              if List.length ctxt = 0 then (
                (*a process expression always compiles to a go function that receives its  channel and other channels from context *)
                Printf.bprintf !compiled "func (%s *%s){\n" c id;
                compile_proc type_map p SeqMap.empty env is_rec;
                Buffer.add_string !compiled "}")
              else (
                Printf.bprintf !compiled "func (%s *%s, " c id;
                compile_lin_ctxt_to_fun_args ctxt type_map;
                Buffer.add_string !compiled "){\n";
                compile_proc type_map p SeqMap.empty env is_rec;
                Buffer.add_string !compiled "}")
          | None -> assert false)
      | None -> assert false)
  | ExecExp exp -> (
      match exp with
      | ProcExp (c, _, opt, _) -> (
          match opt with
          | Some st -> (
              match TypeTable.find_opt type_map st with
              | Some (State (id, _)) -> (
                  match !single_channel with
                  (*The final goroutine that receives in c is necessary to avoid deadlock since  the c process will always end on a close (send(nil)) to be well typed *)
                  | true ->
                      Printf.bprintf !compiled "func main () {\n";
                      Printf.bprintf !compiled
                        "    %s:= init%s(make (chan interface{}))\n" c id;
                      Printf.bprintf !compiled "go func () {\n";
                      Printf.bprintf !compiled "%s.Recv()\n}()\n" c;
                      compile_exp type_map exp env is_rec;
                      Printf.bprintf !compiled "(%s)\n}\n" c
                  | false ->
                      Printf.bprintf !compiled "func main () {\n";
                      Printf.bprintf !compiled "%s := init%s()\n" c id;
                      Printf.bprintf !compiled "go func () {\n";
                      Printf.bprintf !compiled "%s.Recv()\n}()\n" c;
                      compile_exp type_map exp env is_rec;
                      Printf.bprintf !compiled "(%s)\n}\n" c)
              | None -> assert false)
          | None -> assert false)
      (* Since we allow for an executable expression to be a function call, we must interpret it to obtain the resulting procExp which will be executed *)
      | FunApp (_, _) ->
          compile_exp type_map (ExecExp (Interpreter.eval env exp)) env is_rec
      | _ -> error (NonExecutableExpression exp))

(** This function is meant to be used in declaration compilation, which need a return before the expression value, since a declaration is a function
This can be refactored by calling the compile_exp function; instead of just being a copy of it 

Compiles a function body - it compiles an expression meant to be returned, so the resulting string is prepended by the word “return”

@param type_map the type-state map
@param exp the expression to compile
@param functional environment of identifiers and respective functional values, which may needed to evaluate and simplify a function application before compile it
*)
and compile_return_exp type_map exp env =
  match exp with
  | UnitVal -> Buffer.add_string !compiled "return struct{}{}"
  | Num n -> Printf.bprintf !compiled "return %d" n
  | Bool b -> Printf.bprintf !compiled "return %B" b
  | FunDef (v, ty, e', ret) -> (
      match (ty, ret) with
      | Some t1, Some t2 -> (
          match t1 with
          | TUnit ->
              Printf.bprintf !compiled "return func () ";
              compile_type type_map t2;
              Printf.bprintf !compiled "{\n";
              compile_return_exp type_map e' env;
              Printf.bprintf !compiled "}\n"
          | _ ->
              Printf.bprintf !compiled "return func (%s " v;
              compile_type type_map t1;
              Printf.bprintf !compiled ") ";
              compile_type type_map t2;
              Printf.bprintf !compiled "{\n";
              compile_return_exp type_map e' env;
              Printf.bprintf !compiled "}\n")
      | _, _ -> assert false)
  | RecFunDef (_, _, _, _) -> assert false
  | Var v ->
      Buffer.add_string !compiled "return ";
      Buffer.add_string !compiled v
  | BOp (_, _, _) ->
      Buffer.add_string !compiled "return ";
      compile_bop type_map exp env false
  | UOp (_, _) ->
      Buffer.add_string !compiled "return ";
      compile_uop type_map exp env false
  | Let (x, e1, e2) ->
      let nenv = Interpreter.StrMap.add x e1 env in
      let ne2 = Interpreter.eval nenv e2 in
      compile_return_exp type_map ne2 nenv
  | FunApp (e1, _) ->
      Buffer.add_string !compiled "return ";
      compile_exp type_map e1 env false
  | Annot (e', _) -> compile_return_exp type_map e' env
  | Cond (cond, e1, e2) ->
      Buffer.add_string !compiled "if ";
      compile_exp type_map cond env false;
      Buffer.add_string !compiled " {\n";
      compile_return_exp type_map e1 env;
      Buffer.add_string !compiled "\n} else {\n";
      compile_return_exp type_map e2 env;
      Buffer.add_string !compiled "\n}\n"
  | ProcExp (_, _, _, _) ->
      Buffer.add_string !compiled "return ";
      compile_exp type_map exp env false
  | ExecExp _ -> assert false
(*This case is never reached because this function is always called for the expressions of
  declarations, which are never ExecExps*)

(** Compiles a process
    @param type_map the type-state mapping
    @param p teh process to compile
    @param id_map a mapping of process identifiers to current state identifiers
    @param env a functional environment *)
and compile_proc type_map p id_map env is_rec =
  match p with
  | Send (d, exp, _, proc) ->
      (* We can't call d.Send(); d.Send(). Fist time it uses d then changes the name to d1 ...
         First obtain the actual state identifiers in the current scope matching process' channel name*)
      let curr_id = curr_seq_id d id_map in

      (* Then the next identifier in sequence is obtained, as well as a new identifier map, whit the new identifier association *)
      (*let next_id, next_map = next_seq_id d id_map in*)

      (* The compilation result is the assignment to {next_id} of the state that is returned by {curr_id’s} send operation of the expression {ex’s} compilation, appended with the compilation of the continuation process *)
      let next_id, next_map = next_seq_id d id_map in
      Printf.bprintf !compiled "%s := %s.Send(" next_id curr_id;
      compile_exp type_map exp env is_rec;
      Printf.bprintf !compiled ")\n";
      compile_proc type_map proc next_map env is_rec
  | MultiSend (ch, exps, _types, next) ->
      let curr_id = curr_seq_id ch id_map in
      let next_id, next_map = next_seq_id ch id_map in
      Printf.bprintf !compiled "%s := %s.Send(" next_id curr_id;
      compile_multisend_args exps type_map env is_rec;
      Printf.bprintf !compiled ")\n";
      compile_proc type_map next next_map env is_rec
  | Recv (id, d, _, proc) ->
      (* id -> name of the variable where is stored the name of the channel *)
      let curr_id = curr_seq_id d id_map in
      let next_id, next_map = next_seq_id d id_map in
      Printf.bprintf !compiled "%s, %s := %s.Recv()\n" id next_id curr_id;
      compile_proc type_map proc next_map env is_rec
  | MultiRecv (ch, vars, _, next) ->
      let curr_id = curr_seq_id ch id_map in
      let next_id, next_map = next_seq_id ch id_map in
      Buffer.add_string !compiled (String.concat ", " vars);
      Printf.bprintf !compiled ", %s := %s.Recv()\n" next_id curr_id;
      compile_proc type_map next next_map env is_rec
  | Close d ->
      Buffer.add_string !compiled (curr_seq_id d id_map);
      Buffer.add_string !compiled ".Send(nil)\n";
      if is_rec then Buffer.add_string !compiled "break\n"
  | Wait (d, proc) ->
      let curr_id = curr_seq_id d id_map in
      Printf.bprintf !compiled "%s.Recv()\n" curr_id;
      compile_proc type_map proc id_map env is_rec
  | Fwd (opt, c, d) -> (
      match opt with
      | Some sty ->
          Printf.bprintf !compiled "// FWD %s %s Start\n" c d;
          let _ = compile_fwd_stype sty c d type_map id_map [] in
          Printf.bprintf !compiled "// FWD %s %s End\n" c d
      | None -> assert false)
  | Spawn (d, exp, opt, proc, args) -> (
      match opt with
      | Some st -> (
          (* Finding the state matching the session type of the spawned process. The result is the call to the respective state’s initialization function, followed by a goroutine call to the spawn’s compiled expression, appended by the compilation of the continuation process. *)
          match TypeTable.find_opt type_map st with
          | Some (State (id, _)) -> (
              match !single_channel with
              | true ->
                  (* let curr_id = curr_seq_id d id_map in // I can use just d instead of curr_seq_id since the id_map is always empty, since there is no matching id for d in id_map *)
                  if List.length args = 0 then (
                    Printf.bprintf !compiled "%s := init%s(make(chan interface{}))\n" d id;
                    Buffer.add_string !compiled "go ";
                    compile_exp type_map exp env is_rec;
                    Printf.bprintf !compiled "(%s)\n" d;
                    compile_proc type_map proc id_map env is_rec)
                  else (
                    Printf.bprintf !compiled "%s := init%s(make(chan interface{}))\n" d id;
                    Buffer.add_string !compiled "go ";
                    compile_exp type_map exp env is_rec;
                    Printf.bprintf !compiled "(%s, " d;
                    compile_var_list_to_fun_args args id_map;
                    Buffer.add_string !compiled ")\n";
                    compile_proc type_map proc id_map env is_rec)
              | false ->
                  (* let curr_id = curr_seq_id d id_map in // I can use just d instead of curr_seq_id since the id_map is always empty, since there is no matching id for d in id_map *)
                  if List.length args = 0 then (
                    Printf.bprintf !compiled "%s := init%s()\n" d id;
                    Buffer.add_string !compiled "go ";
                    compile_exp type_map exp env is_rec;
                    Printf.bprintf !compiled "(%s)\n" d;
                    compile_proc type_map proc id_map env is_rec)
                  else (
                    Printf.bprintf !compiled "%s := init%s()\n" d id;
                    Buffer.add_string !compiled "go ";
                    compile_exp type_map exp env is_rec;
                    Printf.bprintf !compiled "(%s, " d;
                    compile_var_list_to_fun_args args id_map;
                    Buffer.add_string !compiled ")\n";
                    compile_proc type_map proc id_map env is_rec))
          | None -> assert false)
      | None -> assert false (* Choice/labels are communication of strings *))
  | TailSpawn (d, exp, opt, args, is_recursive, _, _) -> (
      match opt with
      | Some st -> (
          match TypeTable.find_opt type_map st with
          | Some (State (_, _)) ->
              (* Only compile the TailSpawn is  not a recursive call. If is recursive then we must update the arguments, that is done separately *)
              if not is_recursive then
                let curr_id = curr_seq_id d id_map in
                if List.length args = 0 then (
                  compile_exp type_map exp env is_rec;
                  Printf.bprintf !compiled "(%s)\n" curr_id)
                else (
                  compile_exp type_map exp env is_rec;
                  Printf.bprintf !compiled "(%s, " curr_id;
                  compile_var_list_to_fun_args args id_map;
                  Buffer.add_string !compiled ")\n")
              else compile_update_args type_map (copy_id_map id_map) env p
          | None -> assert false)
      | None -> assert false (* Choice/labels are communication of strings *))
  | Choice (d, l) ->
      let curr_id = curr_seq_id d id_map in
      Printf.bprintf !compiled "label := %s.Recv()\n" curr_id;
      Buffer.add_string !compiled "switch label {\n";
      compile_choice_list type_map d l id_map env is_rec;
      Buffer.add_string !compiled "}\n"
  | Label (d, l, proc, opt) -> (
      match opt with
      | Some st -> (
          match TypeTable.find_opt type_map st with
          | Some (State (id, _)) ->
              let curr_id = curr_seq_id d id_map in
              let next_id, next_map = next_seq_id d id_map in
              Printf.bprintf !compiled "%s.Send(\"%s\")\n" curr_id l;
              Printf.bprintf !compiled "%s := %s.ls[\"%s\"].(*%s)\n" next_id curr_id l id;
              compile_proc type_map proc next_map env is_rec
          | None -> assert false)
      | None -> assert false)
  | SendChan (d, e, _, proc) ->
      let curr_id = curr_seq_id d id_map in
      let next_id, next_map = next_seq_id d id_map in
      Printf.bprintf !compiled "%s := %s.Send(%s)\n" next_id curr_id e;
      compile_proc type_map proc next_map env is_rec
  | RecvChan (id, d, opt, proc) -> (
      let curr_id = curr_seq_id d id_map in
      let next_id, next_map = next_seq_id d id_map in
      match !single_channel with
      | true -> (
          let var_id, final_map = next_seq_id id next_map in
          match opt with
          | Some st -> (
              match TypeTable.find_opt type_map st with
              | Some (State (state_id, _)) ->
                  Printf.bprintf !compiled "%s, %s := %s.Recv()\n" id next_id curr_id;
                  Printf.bprintf !compiled "%s := %s.(*%s)\n" var_id id state_id;
                  compile_proc type_map proc final_map env is_rec
              | None -> assert false)
          | None -> assert false)
      | false ->
          Printf.bprintf !compiled "%s, %s := %s.Recv()\n" id next_id curr_id;
          compile_proc type_map proc next_map env is_rec)
  | Print (e, proc) ->
      needs_print := true;
      Buffer.add_string !compiled "fmt.Printf(\"%v\\n\",";
      compile_exp type_map e env is_rec;
      Buffer.add_string !compiled ")\n";
      compile_proc type_map proc id_map env is_rec
  | If (e, p1, p2) ->
      Buffer.add_string !compiled "if ";
      compile_exp type_map e env is_rec;
      Buffer.add_string !compiled " {\n";
      compile_proc type_map p1 (copy_id_map id_map) env is_rec;
      Buffer.add_string !compiled "} else {\n";
      compile_proc type_map p2 (copy_id_map id_map) env is_rec;
      Buffer.add_string !compiled "}\n"

and compile_multisend_args exps type_map env is_rec =
  match exps with
  | [ e ] -> compile_exp type_map e env is_rec
  | hd :: tail ->
      compile_exp type_map hd env is_rec;
      Buffer.add_string !compiled ", ";
      compile_multisend_args tail type_map env is_rec
  | [] -> ()

and compile_var_list_to_fun_args vars id_map =
  match vars with
  | [ v ] -> Buffer.add_string !compiled (curr_seq_id v id_map)
  | v :: tail ->
      Buffer.add_string !compiled (curr_seq_id v id_map);
      Buffer.add_string !compiled ", ";
      compile_var_list_to_fun_args tail id_map
  | [] -> ()

(** This functions is only called in a recursive function. 
      @param tail_spawn TailSpawn responsible for the recursive call *)
and compile_update_args type_map id_map env tail_spawn =
  match tail_spawn with
  | TailSpawn (d, exp, _, spawn_args, _, fun_arg, original_args) ->
      (* Update arguments *)
      Buffer.add_string !compiled "//Update arguments\n";
      (match exp with
      | FunApp (_, e2) -> (
          match e2 with
          | UnitVal -> ()
          | args -> (
              match fun_arg with
              | Some a ->
                  Printf.bprintf !compiled "%s = " a;
                  compile_exp type_map args env true;
                  Buffer.add_string !compiled "\n"
              | None -> ()))
      | _ -> assert false);
      (*Update channels*)
      Buffer.add_string !compiled "//Update channels\n";
      let curr_id = curr_seq_id d id_map in
      Printf.bprintf !compiled "%s = %s\n" d curr_id;
      (*Atualização com argumentos atuais *)
      (* We need to rever the list to be in the same order *)
      compile_list_spawn_args spawn_args (List.rev original_args) id_map
  | _ -> assert false

(** 
Compiles, from the session type of the process in context the appropriate forwarding behavior between processes

Compile the appropriate behavior to redirect messages from one process to another and vice-versa; the big challenge here is the recursion. A recursive fwd compiles to an infinite for cycle.

@param start_c the optional labeled arguments of starting {!c} - identifier of the process that offers the session
@param start_d the optional labeled arguments of starting {!d} - is the identifier of the process of the same type in the linear context

In the end of the cycle the names need to be restored to the original ones. these starting identifiers are needed in the case of recursion, to correctly set process identifiers, when restarting the recursive cycle
@param sty the session type of the process {!d} in context, the type of d
@param c the process offering the session
@param d the process in context
@param the type-state associations
@param the process identifier map
@param a list of recursion variables used to identify already encountered recursive processes
@return false if it does not nothing to the buffer and true otherwise
*)
and compile_fwd_stype ?(start_c = "") ?(start_d = "") sty c d type_map id_map vars =
  match sty with
  (* st is type of d st-> is the next action, we need to see if it is a recursive type*)
  | STSend (_, st) -> (
      let curr_d = curr_seq_id d id_map in
      (* The function starts by obtaining and building necessary process and value identifiers *)
      let next_d, fst_map = next_seq_id d id_map in
      let curr_c = curr_seq_id c fst_map in
      let next_c, snd_map = next_seq_id c fst_map in
      let aux_id = curr_c ^ curr_d in
      let mid_d = curr_c ^ "_" ^ curr_d in
      match st with
      | STRec (v, _) -> (
          match List.find_opt (fun el -> el = v) vars with
          | Some _ ->
              (* In the case that the continuation type is the recursive type, there is a check: if the recursive type has already been encountered (its respective recursion variable exists in the vars environment), care must be taken to set new starting loop values for {c} and {d}, through the use of {start_c} and {start_d} *)
              Printf.bprintf !compiled "%s, %s := %s.Recv()\n" aux_id mid_d curr_d;
              Printf.bprintf !compiled "%s = %s\n%s = %s.Send(%s)\n" start_d mid_d start_c
                curr_c aux_id;
              true
          | None ->
              (* otherwise the function proceeds with redirection as normal *)
              Printf.bprintf !compiled "%s, %s := %s.Recv()" aux_id next_d curr_d;
              Printf.bprintf !compiled "%s := %s.Send(%s)\n" next_c curr_c aux_id;
              compile_fwd_stype ~start_c ~start_d st c d type_map snd_map vars || true)
      | _ ->
          (* If it is not the recursive type, the compilation result is simply a channel
             redirection - receiving in {d}, and sending in {c} *)
          Printf.bprintf !compiled "%s, %s := %s.Recv()\n" aux_id next_d curr_d;
          Printf.bprintf !compiled "%s := %s.Send(%s)\n" next_c curr_c aux_id;
          compile_fwd_stype ~start_c ~start_d st c d type_map snd_map vars || true)
  | STMultiSend (v, st) -> (
      let curr_d = curr_seq_id d id_map in
      let next_d, fst_map = next_seq_id d id_map in
      let curr_c = curr_seq_id c fst_map in
      let next_c, snd_map = next_seq_id c fst_map in
      let aux_id = curr_c ^ curr_d in
      let mid_d = curr_c ^ "_" ^ curr_d in
      let vars = List.mapi (fun i _ -> Printf.sprintf "%s_%d" aux_id i) v in
      let list_vars = String.concat ", " vars in
      match st with
      | STRec (v, _) -> (
          match List.find_opt (fun el -> el = v) vars with
          | Some _ ->
              Printf.bprintf !compiled "%s, %s := %s.Recv()\n" list_vars mid_d curr_d;
              Printf.bprintf !compiled "%s = %s\n%s = %s.Send(%s)\n" start_d mid_d start_c
                curr_c list_vars;
              true
          | None ->
              Printf.bprintf !compiled "%s, %s := %s.Recv()" list_vars next_d curr_d;
              Printf.bprintf !compiled "%s := %s.Send(%s)\n" next_c curr_c list_vars;
              compile_fwd_stype ~start_c ~start_d st c d type_map snd_map vars || true)
      | _ ->
          Printf.bprintf !compiled "%s, %s := %s.Recv()\n" list_vars next_d curr_d;
          Printf.bprintf !compiled "%s := %s.Send(%s)\n" next_c curr_c list_vars;
          compile_fwd_stype ~start_c ~start_d st c d type_map snd_map vars || true)
  | STRecv (_, st) -> (
      let curr_d = curr_seq_id d id_map in
      let next_d, fst_map = next_seq_id d id_map in
      let curr_c = curr_seq_id c fst_map in
      let next_c, snd_map = next_seq_id c fst_map in
      let aux_id = curr_d ^ curr_c in
      let mid_c = curr_c ^ "_" ^ curr_d in
      match st with
      | STRec (v, _) -> (
          match List.find_opt (fun el -> el = v) vars with
          | Some _ ->
              Printf.bprintf !compiled "%s, %s := %s.Recv()\n" aux_id mid_c curr_c;
              Printf.bprintf !compiled "%s = %s\n" start_c mid_c;
              Printf.bprintf !compiled "%s = %s.Send(%s)\n" start_d curr_d aux_id;
              true
          | None ->
              Printf.bprintf !compiled "%s, %s := %s.Recv()\n" aux_id next_c curr_c;
              Printf.bprintf !compiled "%s := %s.Send(%s)\n" next_d curr_d aux_id;
              compile_fwd_stype ~start_c ~start_d st c d type_map snd_map vars || true)
      | _ ->
          Printf.bprintf !compiled "%s, %s := %s.Recv()\n" aux_id next_c curr_c;
          Printf.bprintf !compiled "%s := %s.Send(%s)\n" next_d curr_d aux_id;
          compile_fwd_stype ~start_c ~start_d st c d type_map snd_map vars || true)
  | STMultiRecv (v, st) -> (
      let curr_d = curr_seq_id d id_map in
      let next_d, fst_map = next_seq_id d id_map in
      let curr_c = curr_seq_id c fst_map in
      let next_c, snd_map = next_seq_id c fst_map in
      let aux_id = curr_d ^ curr_c in
      let mid_c = curr_c ^ "_" ^ curr_d in
      let vars = List.mapi (fun i _ -> Printf.sprintf "%s_%d" aux_id i) v in
      let list_vars = String.concat ", " vars in
      match st with
      | STRec (v, _) -> (
          match List.find_opt (fun el -> el = v) vars with
          | Some _ ->
              Printf.bprintf !compiled "%s, %s := %s.Recv()\n" list_vars mid_c curr_c;
              Printf.bprintf !compiled "%s = %s\n" start_c mid_c;
              Printf.bprintf !compiled "%s = %s.Send(%s)\n" start_d curr_d list_vars;
              true
          | None ->
              Printf.bprintf !compiled "%s, %s := %s.Recv()\n" list_vars next_c curr_c;
              Printf.bprintf !compiled "%s := %s.Send(%s)\n" next_d curr_d list_vars;
              compile_fwd_stype ~start_c ~start_d st c d type_map snd_map vars || true)
      | _ ->
          Printf.bprintf !compiled "%s, %s := %s.Recv()\n" list_vars next_c curr_c;
          Printf.bprintf !compiled "%s := %s.Send(%s)\n" next_d curr_d list_vars;
          compile_fwd_stype ~start_c ~start_d st c d type_map snd_map vars || true)
  | STExtChoice l ->
      let curr_d = curr_seq_id d id_map in
      let curr_c = curr_seq_id c id_map in
      let label = curr_d ^ curr_c in
      (*receive label*)
      Printf.bprintf !compiled "%s := %s.Recv()\n" label curr_c;
      (* propagate label *)
      Printf.bprintf !compiled "%s.Send(%s)\n" curr_d label;
      Printf.bprintf !compiled "switch %s {\n" label;
      compile_fwd_stype_list ~start_c ~start_d l c d type_map id_map vars;
      Buffer.add_string !compiled "}\n";
      true
  | STIntChoice l ->
      let curr_d = curr_seq_id d id_map in
      let curr_c = curr_seq_id c id_map in
      let label = curr_c ^ curr_d in
      (* receive label *)
      Printf.bprintf !compiled "%s := %s.Recv()\n" label curr_d;
      (* propagate label *)
      Printf.bprintf !compiled "%s.Send(%s)\n" curr_c label;
      Printf.bprintf !compiled "switch %s {\n" label;
      compile_fwd_stype_list ~start_c ~start_d l c d type_map id_map vars;
      Buffer.add_string !compiled "}\n";
      true
  | STSendChan (_, st2) -> (
      let curr_d = curr_seq_id d id_map in
      let next_d, fst_map = next_seq_id d id_map in
      let curr_c = curr_seq_id c fst_map in
      let next_c, snd_map = next_seq_id c fst_map in
      let aux_id = curr_c ^ curr_d in
      let mid_d = curr_c ^ "_" ^ curr_d in
      match st2 with
      | STRec (v, _) -> (
          match List.find_opt (fun el -> el = v) vars with
          | Some _ ->
              Printf.bprintf !compiled "%s, %s := %s.Recv()\n" aux_id mid_d curr_d;
              Printf.bprintf !compiled "%s = %s\n" start_d mid_d;
              Printf.bprintf !compiled "%s = %s.Send(%s)\n" start_c curr_c aux_id;
              true
          | None ->
              Printf.bprintf !compiled "%s, %s := %s.Recv()\n" aux_id next_d curr_d;
              Printf.bprintf !compiled "%s := %s.Send(%s)\n" next_c curr_c aux_id;
              compile_fwd_stype ~start_c ~start_d st2 c d type_map snd_map vars || true)
      | _ ->
          Printf.bprintf !compiled "%s, %s := %s.Recv()\n" aux_id next_d curr_d;
          Printf.bprintf !compiled "%s := %s.Send(%s)\n" next_c curr_c aux_id;
          compile_fwd_stype ~start_c ~start_d st2 c d type_map snd_map vars || true)
  | STRecvChan (_, st2) -> (
      let curr_d = curr_seq_id d id_map in
      let next_d, fst_map = next_seq_id d id_map in
      let curr_c = curr_seq_id c fst_map in
      let next_c, snd_map = next_seq_id c fst_map in
      let aux_id = curr_d ^ curr_c in
      let mid_c = curr_c ^ "_" ^ curr_d in
      match st2 with
      | STRec (v, _) -> (
          match List.find_opt (fun el -> el = v) vars with
          | Some _ ->
              Printf.bprintf !compiled "%s, %s := %s.Recv()\n" aux_id mid_c curr_c;
              Printf.bprintf !compiled "%s = %s\n" start_c mid_c;
              Printf.bprintf !compiled "%s = %s.Send(%s)\n" start_d curr_d aux_id;
              true
          | None ->
              Printf.bprintf !compiled "%s, %s := %s.Recv()\n" aux_id next_c curr_c;
              Printf.bprintf !compiled "%s := %s.Send(%s)\n" next_d curr_d aux_id;
              compile_fwd_stype ~start_c ~start_d st2 c d type_map snd_map vars || true)
      | _ ->
          Printf.bprintf !compiled "%s, %s := %s.Recv()\n" aux_id next_c curr_c;
          Printf.bprintf !compiled "%s := %s.Send(%s)\n" next_d curr_d aux_id;
          compile_fwd_stype ~start_c ~start_d st2 c d type_map snd_map vars || true)
  | STVar _ -> false
  | STRec (v, _) -> (
      (* Recursive type - unroll the recursive type, creates an infinite loop*)
      (* If the forwarded type is a recursive type, the forwarder code is simply embedded inside an infinite loop*)
      (* To start, the function checks if the type of recursion itself has already been encountered, by checking the vars environment for the respective recursion variable. *)
      match List.find_opt (fun el -> el = v) vars with
      | Some _ -> false
      (* If so, the compilation result is the empty string *)
      | None ->
          (* otherwise the compilation result is a call to {compile_fwd_stype}, with {start_c} and {start_d} set to the identifiers of {c} and {d} in the current scope, with the unfolded session type, and with the recursion variable {v} added to {vars}, embedded within a for loop *)
          let curr_c = curr_seq_id c id_map in
          let curr_d = curr_seq_id d id_map in
          Buffer.add_string !compiled "for {\n";
          let _ =
            compile_fwd_stype ~start_c:curr_c ~start_d:curr_d (unfold sty) c d type_map
              id_map (v :: vars)
          in
          Buffer.add_string !compiled "}\n";
          true)
  | STEnd ->
      (* treats the type of a session that has closed communication. It redirects a session termination signal from process to another. The compilation result contains a “return”, to ensure it breaks out of a for cycle, if the session termination occurs inside a recursive type.*)
      let curr_d = curr_seq_id d id_map in
      let curr_c = curr_seq_id c id_map in
      Printf.bprintf !compiled "%s.Recv()\n" curr_d;
      Printf.bprintf !compiled "%s.Send(nil)\n" curr_c;
      Printf.bprintf !compiled "return\n";
      true
  | STUVar _ -> assert false

(** The function that compiles a list of label-stype pairs. The challenge here is maintaining consistent id usage across cases.
This function can be simplified by simply using hard copy maps (from copy_id_map), instead of keeping track of initial ids and restoring the maps to their initial states.

This function compiles the label-type pairs into a list of matching cases (to be embedded inside a switch). The case label is the label in the aforementioned pair; the case body is the compilation of the type in the pair. 

@param start_c optional argument necessary to set the starting values of {c} in case of recursion
@param start_d optional argument necessary to set the starting values of {d} in case of recursion
@param l a list of label-type map
@param c the channel the session is offered in
@param d the channel in context
@param type_map the type_state map
@param id_map the process-identifier association map
@param vars an environment containing recursion variables pertaining to already encountered recursive types.
*)
and compile_fwd_stype_list ?(start_c = "") ?(start_d = "") l c d type_map id_map vars =
  match l with
  | (str, sty) :: tail -> (
      match TypeTable.find_opt type_map sty with
      | Some (State (state, _)) -> (
          match SeqMap.find_opt d id_map with
          (* record starting state of the id_map for the next case *)
          | Some d_ref -> (
              match SeqMap.find_opt c id_map with
              | Some c_ref ->
                  let init_d = !d_ref in
                  let init_c = !c_ref in
                  let curr_d = curr_seq_id d id_map in
                  let next_d, fst_map = next_seq_id d id_map in
                  let curr_c = curr_seq_id c fst_map in
                  let next_c, snd_map = next_seq_id c fst_map in
                  let reset = Buffer.length !compiled in
                  Printf.bprintf !compiled "case \"%s\":\n" str;
                  Printf.bprintf !compiled "%s := %s.ls[\"%s\"].(*%s)\n" next_d curr_d str
                    state;
                  Printf.bprintf !compiled "%s := %s.ls[\"%s\"].(*%s)\n" next_c curr_c str
                    state;

                  let continuation =
                    compile_fwd_stype ~start_c ~start_d sty c d type_map snd_map vars
                  in
                  if not continuation then Buffer.truncate !compiled reset else ();
                  compile_fwd_stype_list ~start_c ~start_d tail c d type_map
                    (SeqMap.add c (ref init_c) (SeqMap.add d (ref init_d) id_map))
                    vars
              | None ->
                  let init_d = !d_ref in
                  let curr_d = curr_seq_id d id_map in
                  let next_d, fst_map = next_seq_id d id_map in
                  let curr_c = curr_seq_id c fst_map in
                  let next_c, snd_map = next_seq_id c fst_map in
                  let reset = Buffer.length !compiled in
                  Printf.bprintf !compiled "case \"%s\":\n" str;
                  Printf.bprintf !compiled "%s := %s.ls[\"%s\"].(*%s)\n" next_d curr_d str
                    state;
                  Printf.bprintf !compiled "%s := %s.ls[\"%s\"].(*%s)\n" next_c curr_c str
                    state;
                  (*Printf.bprintf !compiled_decls "%s" continuation;*)
                  let continuation =
                    compile_fwd_stype ~start_c ~start_d sty c d type_map snd_map vars
                  in

                  if not continuation then Buffer.truncate !compiled reset else ();

                  compile_fwd_stype_list ~start_c ~start_d tail c d type_map
                    (SeqMap.remove c (SeqMap.add d (ref init_d) id_map))
                    vars)
          | None -> (
              match SeqMap.find_opt c id_map with
              | Some c_ref ->
                  let curr_d = curr_seq_id d id_map in
                  let init_c = !c_ref in
                  let next_d, fst_map = next_seq_id d id_map in
                  let curr_c = curr_seq_id c fst_map in
                  let next_c, snd_map = next_seq_id c fst_map in
                  let reset = Buffer.length !compiled in
                  Printf.bprintf !compiled "case \"%s\":\n" str;
                  Printf.bprintf !compiled "%s := %s.ls[\"%s\"].(*%s)\n" next_d curr_d str
                    state;
                  Printf.bprintf !compiled "%s := %s.ls[\"%s\"].(*%s)\n" next_c curr_c str
                    state;
                  let continuation =
                    compile_fwd_stype ~start_c ~start_d sty c d type_map snd_map vars
                  in
                  if not continuation then Buffer.truncate !compiled reset else ();
                  compile_fwd_stype_list ~start_c ~start_d tail c d type_map
                    (SeqMap.add c (ref init_c) (SeqMap.remove d id_map))
                    vars
              | None ->
                  let curr_d = curr_seq_id d id_map in
                  let next_d, fst_map = next_seq_id d id_map in
                  let curr_c = curr_seq_id c fst_map in
                  let next_c, snd_map = next_seq_id c fst_map in
                  let reset = Buffer.length !compiled in
                  Printf.bprintf !compiled "case \"%s\":\n" str;
                  Printf.bprintf !compiled "%s := %s.ls[\"%s\"].(*%s)\n" next_d curr_d str
                    state;
                  Printf.bprintf !compiled "%s := %s.ls[\"%s\"].(*%s)\n" next_c curr_c str
                    state;
                  let continuation =
                    compile_fwd_stype ~start_c ~start_d sty c d type_map snd_map vars
                  in
                  if not continuation then Buffer.truncate !compiled reset else ();
                  compile_fwd_stype_list ~start_c ~start_d tail c d type_map
                    (SeqMap.remove c (SeqMap.remove d id_map))
                    vars))
      | None -> assert false)
  | [] -> ()

(** We compile a choice session using a message-label map, where each label is associated with an appropriately initialized representation of the corresponding branch type *)
and compile_choice_list type_map d ls id_map env is_rec =
  match ls with
  | (l, (proc, opt)) :: tail -> (
      match opt with
      | Some st -> (
          match TypeTable.find_opt type_map st with
          | Some (State (id, _)) -> (
              match SeqMap.find_opt d id_map with
              (* record starting state of the id_map for the next case *)
              | Some start_ref ->
                  let start_num = !start_ref in
                  let curr_id = curr_seq_id d id_map in

                  Printf.bprintf !compiled "case \"%s\":\n" l;
                  let next_id, next_map = next_seq_id d id_map in
                  Printf.bprintf !compiled "%s := %s.ls[\"%s\"].(*%s)\n" next_id curr_id l
                    id;
                  compile_proc type_map proc (copy_id_map next_map) env is_rec;
                  compile_choice_list type_map d tail
                    (SeqMap.add d (ref start_num) id_map)
                    env is_rec
              | None ->
                  let curr_id = curr_seq_id d id_map in
                  Printf.bprintf !compiled "case \"%s\" :\n" l;
                  let next_id, next_map = next_seq_id d id_map in
                  Printf.bprintf !compiled "%s := %s.ls[\"%s\"].(*%s)\n" next_id curr_id l
                    id;
                  compile_proc type_map proc (copy_id_map next_map) env is_rec;
                  compile_choice_list type_map d tail (SeqMap.remove d id_map) env is_rec
                  (* restore starting state of the id_map for the next case *))
          | None -> assert false)
      | None -> assert false)
  | [] -> ()

and compile_bop type_map e env is_rec =
  match e with
  | BOp (bop, e1, e2) -> (
      match bop with
      | Mul ->
          Buffer.add_string !compiled "(";
          compile_exp type_map e1 env is_rec;
          Buffer.add_string !compiled " * ";
          compile_exp type_map e2 env is_rec;
          Buffer.add_string !compiled ")"
      | Div ->
          Buffer.add_string !compiled "(";
          compile_exp type_map e1 env is_rec;
          Buffer.add_string !compiled " / ";
          compile_exp type_map e2 env is_rec;
          Buffer.add_string !compiled ")"
      | Add ->
          Buffer.add_string !compiled "(";
          compile_exp type_map e1 env is_rec;
          Buffer.add_string !compiled " + ";
          compile_exp type_map e2 env is_rec;
          Buffer.add_string !compiled ")"
      | Sub ->
          Buffer.add_string !compiled "(";
          compile_exp type_map e1 env is_rec;
          Buffer.add_string !compiled " - ";
          compile_exp type_map e2 env is_rec;
          Buffer.add_string !compiled ")"
      | And ->
          Buffer.add_string !compiled "(";
          compile_exp type_map e1 env is_rec;
          Buffer.add_string !compiled " && ";
          compile_exp type_map e2 env is_rec;
          Buffer.add_string !compiled ")"
      | Or ->
          Buffer.add_string !compiled "(";
          compile_exp type_map e1 env is_rec;
          Buffer.add_string !compiled " || ";
          compile_exp type_map e2 env is_rec;
          Buffer.add_string !compiled ")"
      | Lesser ->
          Buffer.add_string !compiled "(";
          compile_exp type_map e1 env is_rec;
          Buffer.add_string !compiled " < ";
          compile_exp type_map e2 env is_rec;
          Buffer.add_string !compiled ")"
      | Greater ->
          Buffer.add_string !compiled "(";
          compile_exp type_map e1 env is_rec;
          Buffer.add_string !compiled " > ";
          compile_exp type_map e2 env is_rec;
          Buffer.add_string !compiled ")"
      | Equals ->
          Buffer.add_string !compiled "(";
          compile_exp type_map e1 env is_rec;
          Buffer.add_string !compiled " == ";
          compile_exp type_map e2 env is_rec;
          Buffer.add_string !compiled ")")
  | _ -> assert false

and compile_uop type_map e env is_rec =
  match e with
  | UOp (uop, e') -> (
      match uop with
      | Neg ->
          Buffer.add_string !compiled "(-";
          compile_exp type_map e' env is_rec;
          Buffer.add_string !compiled ")"
      | Not ->
          Buffer.add_string !compiled "(!";
          compile_exp type_map e' env is_rec;
          Buffer.add_string !compiled ")")
  | _ -> assert false

(** Compiles a recursive expression - its is called for the body of a recursive function definition  *)
let compile_rec_exp type_map exp env =
  match exp with
  | ProcExp (c, p, opt, ctxt) -> (
      (* c -> name of the channel
         p -> process opt -> name of the process
         ctxt -> things that the process uses *)
      match opt with
      | Some st -> (
          (* The map's key is the type itself and the values is a string whit the correspondent Go type *)
          match TypeTable.find_opt type_map st with
          | Some (State (id, _)) ->
              if List.length ctxt = 0 then (
                (*a process expression always compiles to a go function that receives its  channel and other channels from context *)
                Printf.bprintf !compiled "func (%s *%s){\n" c id;
                Buffer.add_string !compiled "for {\n ";
                compile_proc type_map p SeqMap.empty env true;
                Buffer.add_string !compiled "}\n}\n")
              else (
                Printf.bprintf !compiled "func (%s *%s, " c id;
                compile_lin_ctxt_to_fun_args ctxt type_map;
                Buffer.add_string !compiled "){\n";
                Buffer.add_string !compiled "for {\n ";
                compile_proc type_map p SeqMap.empty env true;
                (* The arguments are updated in the compilation of the process, when the TailSpawn is reached *)
                Buffer.add_string !compiled "}\n}\n")
          | None -> assert false)
      | None -> assert false)
  | _ -> compile_exp type_map exp env true

(** 
  @param map maps types to their names
  @param decl declaration to compile
  @param env new maps with names to expressions
  Generates a compiled version of a declaration *)
let compile_decl map decl env =
  match decl with
  | Decl (x, _, exp) -> (
      match exp with
      | FunDef (v, Some arg, body, Some ret) -> (
          match arg with
          | TUnit ->
              Printf.bprintf !compiled "func %s()" x;
              compile_type map ret;
              Buffer.add_string !compiled " {\n ";
              compile_return_exp map body env;
              Buffer.add_string !compiled "}\n";
              Interpreter.StrMap.add x exp env
          | _ ->
              Printf.bprintf !compiled "func %s(%s " x v;
              compile_type map arg;
              Buffer.add_string !compiled ") ";
              compile_type map ret;
              Buffer.add_string !compiled " {\n ";
              compile_return_exp map body env;
              Buffer.add_string !compiled "}\n";
              Interpreter.StrMap.add x exp env)
      | RecFunDef (v, Some arg, body, Some ret) -> (
          match arg with
          | TUnit ->
              Printf.bprintf !compiled "func %s()" x;
              compile_type map ret;
              Buffer.add_string !compiled " {\n ";
              Buffer.add_string !compiled "return ";
              compile_rec_exp map body env;
              Buffer.add_string !compiled "}\n";
              Interpreter.StrMap.add x exp env
          | _ ->
              Printf.bprintf !compiled "func %s(%s " x v;
              compile_type map arg;
              Buffer.add_string !compiled ") ";
              compile_type map ret;
              Buffer.add_string !compiled " {\n ";
              (*compile_return_exp map body env;*)
              Buffer.add_string !compiled "return ";
              (* Compile within a cycle *)
              compile_rec_exp map body env;
              Buffer.add_string !compiled "}\n";
              Interpreter.StrMap.add x exp env)
      | _ -> env)

(* Case of custom type declaration; has no effect on compilation result *)

(**
 Entry function for compiling a program into Go code
@param prog program to compile
@param filename name of the file to write the compiled code to
@param single_channel whether to use single or multiple channels for communication
@param is_multisend_struct whether to use the MultiSend struct or the list
@param compute_times flag to determine whether to compute the time of execution of the program or not - when computing the times we must not write to the file
@param filename name of the file to write the compiled code to

The function compiles a program ({!tag: prog}) into Go code and writes it to a file ({!filename}). The {!single_channel} argument determines whether the compiled code uses single or multiple channels for communication.
The prog argument is expected to be a Prog type, which contains a list of declarations ({!ldecls}) and an expression ({!e}).
The function has several steps:

-> It creates a map of types and states - a state tree - from the program’s declarations and expressions using the {!method:make_state_trees_from_exp} function.

-> It generates a preamble for the Go code using the {!compile_from_state_list} function and either {!compile_state_single_channel} or {!compile_state_multi_channel} depending on the {!single_channel} argument. In the {b preamble generation} phase every session type of the program is converted into a set of types and associated methods in the host language.

-> In the {b code generation} step the instructions of the program are compiled into Go code

-> It compiles each declaration in the program using the {!compile_decl} function and returns a string of Go code and an environment map. The environment is used to keep track of the variables and their types.

-> It compiles the main expression of the program using the {!compile_exp} function and returns a string of Go code.

-> It opens an output file with the given filename and writes the package name, imports, preamble, declarations and main expression to it. Then it closes the file. *)
let compile_prog prog is_single_channel is_multisend_struct compute_times filename =
  single_channel := is_single_channel;
  compile_with_struct := is_multisend_struct;
  let filename_compile = Printf.sprintf "./%s.go" filename in
  match prog with
  | Prog (ldecls, e) ->
      (* Preamble creation *)
      (* In tge preamble generation *)
      (* This code creates a state-type mapping by iterating through a list of declarations (ldecls) and using the make_state_trees_from_exp function to create a mapping of all the states that appear in the expression associated with each declaration. The List.fold_left function is used to perform the iteration, with an initial value of an empty type table created with a capacity of 50.*)
      (* The first step in the preamble generation phase is generation a map of session types to states, from any declarations in the program, as well from the main expression to execute - this involves creating new states from their respective session types *)
      let stype_state_map =
        (*The function passed to List.fold_left takes two arguments: a map (the first argument, which is initially an empty type table), and a declaration (decl). For each declaration, the function extracts the associated expression (exp) and uses make_state_trees_from_exp to generate a mapping of states that appear in the expression. The resulting mapping is then returned as the new value of the map argument, to be used as the next value for the next iteration. After all declarations have been processed, the final value of the map is returned as the state-type mapping. *)
        List.fold_left
          (fun map decl ->
            match decl with Decl (_, _, exp) -> make_state_trees_from_exp map exp)
          (TypeTable.create 50) ldecls
      in
      (* In main can appear new type declarations *)
      let final_stype_state_map = make_state_trees_from_exp stype_state_map e in
      (* The !state_list expression returns the current contents of the state_list - like a
         pointer := is for assignment *)
      let state_list = ref [] in
      TypeTable.iter
        (fun _ state -> state_list := state :: !state_list)
        final_stype_state_map;

      (* Debug print *)
      Logs.debug (fun m -> m "Printing state list: ");
      TypeTable.iter
        (fun st (State (id, _)) ->
          Logs.info (fun m -> m "%s %s" (Printer.string_from_stype st) id))
          (*print_endline @@ Printer.string_from_stype st ^ " " ^ id)*)
        final_stype_state_map;

      Buffer.add_string !compiled "//Preamble generation\n";
      (match !single_channel with
      | true ->
          compile_from_state_list !state_list final_stype_state_map
            compile_state_single_channel
      | false ->
          compile_from_state_list !state_list final_stype_state_map
            compile_state_multi_channel);

      (* Declaration list compilation *)
      Buffer.add_string !compiled "//Declaration list compilation\n";
      let env =
        List.fold_left
          (fun env decl ->
            let nenv = compile_decl final_stype_state_map decl env in
            nenv)
          Interpreter.StrMap.empty ldecls
      in

      (* Put it all together *)
      Buffer.add_string !compiled "//Main compilation\n";
      compile_exp final_stype_state_map e env false;

      if not compute_times then (
        (* Write to file *)
        let oc = open_out filename_compile in
        Printf.fprintf oc "package main\n";
        if !needs_print then Printf.fprintf oc "import \"fmt\"\n";
        if not is_single_channel then Printf.fprintf oc "import \"sync\"\n";
        Buffer.output_buffer oc !compiled;
        close_out oc)
