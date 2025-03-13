open Syntax
open Typechecker
open Printer 

(* A session amounts to a state machine, with each state representing a session type. The transitions are the sending/receiving of data. 
   So that's how we will operate; we'll first build a state machine for each stype present in the ast, and we'll use that for compilation. 
   
   Three types of states: simple send/receive; a choice between several alternatives end of communication.
   *)
type state_body =
  | Comm of ty * state ref
  | Choice of (string * state ref) list
  | End
and state = 
  | State of string * state_body
  
(* Id generation *)
(* Since go doesn't allow for the redefinition of ids as another type, we must generate 'intermediate' ids for each state. *)
module SeqMap = Map.Make(String)

let curr_seq_id id map = 
  match SeqMap.find_opt id map with
  | Some num_ref -> id ^ string_of_int !num_ref
  | None -> id

let next_seq_id id map = 
  match SeqMap.find_opt id map with
  | None -> let num_ref = ref 0 in 
              let n_map = SeqMap.add id num_ref map in
                curr_seq_id id n_map, n_map
  | Some num_ref -> incr num_ref; curr_seq_id id map, map

let copy_id_map map = 
  let n_map = ref SeqMap.empty in 
    SeqMap.iter (fun k v -> let _ = n_map := SeqMap.add k (ref !v) !n_map in ()) map;
    !n_map

(* Map that contains stype->state associations to keep track of *)
(* Used to maintain a map of stypes and their respective states *)
module StypeKey  = 
  struct 
    type t = stype
    let hash = fun _ -> 1
    let equal a b = subtyping [] a b || subtyping [] b a         

  end
module TypeTable = Hashtbl.Make (StypeKey)

(* Module that keeps stype->id associations to keep track of state names*)
module TypeStore : sig
  val get_id : stype -> string
end = struct (* TODO make this not static *)
  let make_fresh_id =
    let id_curr = ref (-1) in
      fun () -> (incr id_curr; "_state_" ^ (string_of_int !id_curr))

  let hash_table = 
    let init_table = ref (TypeTable.create 50) in 
      fun () -> !init_table

  let get_id stype = 
    let table = hash_table () in
      begin match TypeTable.find_opt table stype with
      | Some id -> id
      | None -> let new_id = make_fresh_id () in TypeTable.replace table stype new_id; new_id
      end
end

(* Make a state from a given stype *)
(* map: the stype->state map
   env: an environment containing previously found recursion variables, and references that will eventually 
   lead to the state that matches the start of the respective recursion *)
let rec make_state map env stype = 
  match TypeTable.find_opt map stype with
  | Some _ -> map
  | None -> begin match stype with
            | STSend (t, st) -> let state_id = TypeStore.get_id stype in
                                  begin match st with 
                                  | STRec (v, _) -> begin match List.assoc_opt v env with 
                                                    | Some dummy_ref -> let this_state = State (state_id, Comm (t, dummy_ref)) in
                                                                          TypeTable.replace map stype this_state; map
                                                    | None -> let new_map = make_state map env st in 
                                                                let this_state = State (state_id, Comm (t, ref (TypeTable.find new_map st))) in
                                                                  TypeTable.replace new_map stype this_state; new_map
                                                    end
                                  | _ -> let new_map = make_state map env st in 
                                          let this_state = State (state_id, Comm (t, ref (TypeTable.find new_map st))) in
                                            TypeTable.replace new_map stype this_state; new_map
                                  end
            | STRecv (t, st) -> let state_id = TypeStore.get_id stype in
                                begin match st with 
                                | STRec (v, _) -> begin match List.assoc_opt v env with 
                                                  | Some dummy_ref -> let this_state = State (state_id, Comm (t, dummy_ref)) in
                                                                        TypeTable.replace map stype this_state; map
                                                  | None -> let new_map = make_state map env st in 
                                                              let this_state = State (state_id, Comm (t, ref (TypeTable.find new_map st))) in
                                                                TypeTable.replace new_map stype this_state; new_map
                                                  end
                                | _ -> let new_map = make_state map env st in 
                                        let this_state = State (state_id, Comm (t, ref (TypeTable.find new_map st))) in
                                          TypeTable.replace new_map stype this_state; new_map
                                end
            | STEnd -> let state_id = TypeStore.get_id stype in 
                        TypeTable.replace map stype (State (state_id, End)); map
            | STExtChoice l -> let state_id = TypeStore.get_id stype in
                                let n_map, state_pair_list = make_state_list map env l in
                                  let state = State (state_id, Choice (state_pair_list)) in
                                    TypeTable.replace n_map stype state; n_map
            | STIntChoice l -> let state_id = TypeStore.get_id stype in
                                let n_map, state_pair_list = make_state_list map env l in
                                  let state = State (state_id, Choice (state_pair_list)) in
                                  TypeTable.replace n_map stype state; n_map
            | STSendChan (st1, st2) ->  let state_id = TypeStore.get_id stype in
                                        begin match st2 with 
                                        | STRec (v, _) -> begin match List.assoc_opt v env with 
                                                          | Some dummy_ref -> let this_state = State (state_id, Comm (TProc (st1, []), dummy_ref)) in
                                                                                TypeTable.replace map stype this_state; map
                                                          | None -> let new_map = make_state map env st2 in
                                                                      let this_state = State (state_id, Comm (TProc (st1, []), ref (TypeTable.find new_map st2))) in
                                                                        TypeTable.replace new_map stype this_state; new_map
                                                          end
                                        | _ -> let new_map = make_state map env st2 in
                                                let this_state = State (state_id, Comm (TProc (st1, []), ref (TypeTable.find new_map st2))) in
                                                  TypeTable.replace new_map stype this_state; new_map
                                        end
            | STRecvChan (st1, st2) ->  let state_id = TypeStore.get_id stype in
                                        begin match st2 with 
                                        | STRec (v,_) -> begin match List.assoc_opt v env with 
                                                        | Some dummy_ref -> let this_state = State (state_id, Comm (TProc (st1, []), dummy_ref)) in
                                                                              TypeTable.replace map stype this_state; map
                                                        | None -> let new_map = make_state map env st2 in 
                                                                    let this_state = State (state_id, Comm (TProc (st1, []), ref (TypeTable.find new_map st2))) in
                                                                      TypeTable.replace new_map stype this_state; new_map
                                                        end
                                        | _ -> let new_map = make_state map env st2 in 
                                                let this_state = State (state_id, Comm (TProc (st1, []), ref (TypeTable.find new_map st2))) in
                                                  TypeTable.replace new_map stype this_state; new_map
                                        end
            | STVar _ -> assert false  (* this case is never reached *)
            | STRec (v, st) -> begin match List.assoc_opt v env with
                               | Some _ -> map
                               | None -> let n_state = State ("%", End) in
                                          let start_ref = ref (n_state) in
                                            let nenv = (v, start_ref)::env in
                                              let unfolded_st = subst_stype stype v st in
                                              let new_map = make_state map nenv unfolded_st in
                                                  start_ref :=  TypeTable.find new_map unfolded_st;
                                                  TypeTable.replace new_map stype !start_ref; new_map
                               end
            | STUVar _ -> assert false
            end
and make_state_list map env list =
  let fold_fun = fun (curr_map, pairs) (l, sty) ->
          begin match sty with 
          | STRec (v, _) -> begin match List.assoc_opt v env with 
                            | Some dummy_ref -> (curr_map, (l, dummy_ref)::pairs)
                            | None -> let n_map = make_state curr_map env sty in
                                        (n_map, (l, ref (TypeTable.find n_map sty))::pairs) 
                            end
          | _ -> let n_map = make_state curr_map env sty in
                  (n_map, (l, ref (TypeTable.find n_map sty))::pairs)
          end
  in (List.fold_left fold_fun (map, []) list)  

(* Traverse an exp AST and make a list of all occurrences of states (made from stypes) *)  
let rec make_state_trees_from_exp map e = 
  match e with
  | UnitVal
  | Num (_) 
  | Bool (_) -> map
  | FunDef (_,_, e', _) -> make_state_trees_from_exp map e'
  | Var _ -> map
  | BOp (_, e1, e2) -> let fst_map = make_state_trees_from_exp map e1 in
                          make_state_trees_from_exp fst_map e2
  | UOp (_, e') -> make_state_trees_from_exp map e'
  | Let (_, e1, e2) -> let fst_map = make_state_trees_from_exp map e1 in
                        make_state_trees_from_exp fst_map e2
  | FunApp (e1, e2) -> let fst_map = make_state_trees_from_exp map e1 in
                        make_state_trees_from_exp fst_map e2
  | Annot (e', _) -> make_state_trees_from_exp map e'
  | Cond (cond, e1 ,e2) -> let fst_map = make_state_trees_from_exp map cond in
                            let snd_map = make_state_trees_from_exp fst_map e1 in
                              make_state_trees_from_exp snd_map e2
  | ProcExp (_, p, opt, _) -> begin match opt with
                                | Some st -> let fst_map = make_state map [] st in
                                              make_state_trees_from_proc fst_map p
                                | None -> assert false
                                end
  | ExecExp exp -> make_state_trees_from_exp map exp
and make_state_trees_from_proc map p = 
  match p with
  | Send (_, exp, _, proc) -> let fst_map = make_state_trees_from_exp map exp in
                                make_state_trees_from_proc fst_map proc
  | Recv (_, _, _, proc) -> make_state_trees_from_proc map proc
  | Close _ -> map
  | Wait (_, proc) -> make_state_trees_from_proc map proc
  | Fwd (_, _, _) -> map
  | Spawn (_, exp, _, proc, _) -> let fst_map = make_state_trees_from_exp map exp in
                              make_state_trees_from_proc fst_map proc
  | Choice (_, l) -> let fold_fun = fun curr_map (_, (proc, _)) -> make_state_trees_from_proc curr_map proc in List.fold_left fold_fun map l
  | Label (_, _, proc, _) -> make_state_trees_from_proc map proc
  | SendChan (_, _, _, proc) -> make_state_trees_from_proc map proc
  | RecvChan (_, _, _, proc) -> make_state_trees_from_proc map proc
  | Print (exp, proc) -> let fst_map = make_state_trees_from_exp map exp in
                          make_state_trees_from_proc fst_map proc
  | If (e, p1, p2) -> let fst_map = make_state_trees_from_exp map e in
                        let snd_map = make_state_trees_from_proc fst_map p1 in
                          make_state_trees_from_proc snd_map p2

(* Auxiliary boilerplate compile functions *)  
(* Multichannel boilerplate *)
let make_from_comm_template_multi_channel name ty next = 
  "type " ^ name ^ " struct {
    c chan " ^ ty ^ "
    next *" ^ next ^ "
    mtx sync.Mutex
  }
  func init" ^ name ^ "() *" ^ name ^ " { return &" ^ name ^ "{ make(chan " ^ ty ^ "), nil, sync.Mutex{} } } 
  func (x *" ^ name ^ ") Send(v " ^ ty ^ ") *" ^ next ^ " { x.mtx.Lock(); if x.next == nil { x.next = init" ^ next ^ "() }; x.mtx.Unlock(); x.c <- v; return " ^ (if name != next then "x.next" else "x") ^ "}
  func (x *" ^ name ^ ") Recv() (" ^ ty ^ ", *" ^ next ^ ") { x.mtx.Lock(); if x.next == nil { x.next = init" ^ next ^ "() }; x.mtx.Unlock();  return <-x.c," ^ (if name != next then "x.next" else "x") ^ "}

  "    

let make_from_end_template_multi_channel name = 
  "type " ^ name ^ " struct {
    c chan interface{}
  }
  func init" ^ name ^ "() *" ^ name ^ " { return &" ^ name ^ "{ make(chan interface{}) } } 
  func (x *" ^ name ^ ") Send(v interface{}) { x.c <- v }
  func (x *" ^ name ^ ") Recv() interface{} { return <-x.c }

  "

let rec make_from_label_pairs_multi_channel label_pairs =
  match label_pairs with
  | (l, rf)::tail -> begin match !rf with
                      | State (next_id, _) -> "\tm[\"" ^ l ^ "\"] = init" ^ next_id ^ "()\n" ^ (make_from_label_pairs_multi_channel tail)
                    end
  | [] -> ""  

let make_from_choice_template_multi_channel name label_pairs = 
  "type " ^ name ^ " struct {
    c  chan string
    ls map[string]interface{}
  }
  func init" ^ name ^ "() *" ^ name ^ " { m := make(map[string]interface{})\n" ^ (make_from_label_pairs_multi_channel label_pairs) ^ "\treturn &" ^ name ^ "{make(chan string), m} }
  func (x *" ^ name ^ ") Send(v string) { x.c <- v }
  func (x *" ^ name ^ ") Recv() string  { return <-x.c }

  "

(* Single channel boilerplate *)
let make_from_comm_template_single_channel name ty next = 
  "type " ^ name ^ " struct {
    c chan " ^ "interface{}" ^ "
    next *" ^ next ^ "
  }
  
  func init" ^ name ^ "(c chan interface{}) *" ^ name ^ " { return &" ^ name ^ "{ c, nil } } 
  func (x *" ^ name ^ ") Send(v " ^ ty ^ ") *" ^ next ^ " { if x.next == nil { x.next = init" ^ next ^ "(x.c) }; x.c <- v; return " ^ (if name != next then "x.next" else "x") ^ "}
  func (x *" ^ name ^ ") Recv() (" ^ ty ^ ", *" ^ next ^ ") { if x.next == nil { x.next = init" ^ next ^ "(x.c) }; return (<-x.c).(" ^ ty ^ ")," ^ (if name != next then "x.next" else "x") ^ "}

  "    

let make_from_end_template_single_channel name = 
  "type " ^ name ^ " struct {
    c chan interface{}
  }
  func init" ^ name ^ "(c chan interface{}) *" ^ name ^ " { return &" ^ name ^ "{ c } } 
  func (x *" ^ name ^ ") Send(v interface{}) { x.c <- v }
  func (x *" ^ name ^ ") Recv() interface{} { return <-x.c }

  "

let rec make_from_label_pairs_single_channel label_pairs =
  match label_pairs with
  | (l, rf)::tail -> begin match !rf with
                      | State (next_id, _) -> "\tm[\"" ^ l ^ "\"] = init" ^ next_id ^ "( c )\n" ^ (make_from_label_pairs_single_channel tail)
                    end
  | [] -> ""

let make_from_choice_template_single_channel name label_pairs = 
  "type " ^ name ^ " struct {
    c  chan interface{}
    ls map[string]interface{}
  }
  func init" ^ name ^ "(c chan interface{}) *" ^ name ^ " { m := make(map[string]interface{})\n" ^ (make_from_label_pairs_single_channel label_pairs) ^ "\treturn &" ^ name ^ "{ c, m } }
  func (x *" ^ name ^ ") Send(v string) { x.c <- v }
  func (x *" ^ name ^ ") Recv() string  { return (<-x.c).(string) }

  "

(* Preamble compilation functions in earnest *)
let rec compile_type type_map ty = 
  match ty with
  | TUnit -> "interface{}"
  | TNum -> "int"
  | TBool -> "bool"
  | TFun (t1, t2) -> begin match t1 with 
                    | TUnit -> "func () " ^ (compile_type type_map t2)
                    | _ -> "func (_x " ^ (compile_type type_map t1) ^ ") " ^ (compile_type type_map t2)
                    end
  | TProc (st, ctxt) -> begin match TypeTable.find_opt type_map st with
                        | Some State (id, _) -> if List.length ctxt = 0 then 
                                                  "func (_x *" ^ id ^ ")"
                                                else
                                                  "func (_x *" ^ id ^ ", " ^ compile_lin_ctxt_to_fun_args ctxt type_map ^ ")"
                        | None -> assert false
                        end
  | TVar v -> v (* This case is never actually reached if, as expected, all TVars have previously been expanded into their primitive equivalents *)
and compile_lin_ctxt_to_fun_args ctxt type_map = 
  match ctxt with 
  | [(c, st)] -> begin match TypeTable.find_opt type_map st with
                | Some State (id, _) -> c ^ " *" ^ id
                | None -> assert false
                end
  | (c, st)::tail -> begin match TypeTable.find_opt type_map st with
                    | Some State (id, _) -> c ^ " *" ^ id ^ ", " ^ compile_lin_ctxt_to_fun_args tail type_map
                    | None -> assert false
                    end
  | [] -> "" 
                    
(* The compile_statefun may be the one that compiles with for a single channel 
   or the one that compiles for multichannel.
   *)
let rec compile_from_state_list state_list type_map compile_statefun = 
  (* let str = ref "" in 
     Seq.iter (fun state -> str := !str ^ (compile_statefun type_map state)) state_list;
    !str *)
  match state_list with
  | state::tail -> (compile_statefun type_map state) ^ (compile_from_state_list tail  type_map compile_statefun)
  | [] -> ""
  
(* Multichannel compilation *)
let compile_state_multi_channel type_map state = 
match state with 
| State (id, state_body) -> begin match state_body with
                            | Comm (t, r) -> begin match !r with 
                                            | State (next_id, _) -> begin match t with
                                                                | TProc (st, _) -> (make_from_comm_template_multi_channel id ("*" ^ (TypeStore.get_id st)) next_id)
                                                                | _ -> (make_from_comm_template_multi_channel id (compile_type type_map t) next_id)
                                                                end
                                            end
                            | Choice l -> (make_from_choice_template_multi_channel id l)
                            | End -> make_from_end_template_multi_channel id
                            end

(* Single channel compilation *)
let compile_state_single_channel type_map state = 
  match state with 
  | State (id, state_body) -> begin match state_body with
                              | Comm (t, r) -> begin match !r with 
                                              | State (next_id, _) -> begin match t with
                                                                      | TProc (_, _) -> (make_from_comm_template_single_channel id "interface{}" next_id)
                                                                      | _ -> (make_from_comm_template_single_channel id (compile_type type_map t) next_id)
                                                                      end
                                              end
                                 
                              | Choice l -> (make_from_choice_template_single_channel id l)
                              | End -> make_from_end_template_single_channel id
                              end

(* Instructions compilation *)
(* The single_channel parameter indicates whether to compile for single channel or multi channel *)
let rec compile_exp single_channel type_map exp env =
  match exp with
  | UnitVal -> "struct{}{}"
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | FunDef (v, ty, e', ret) -> begin match ty, ret with 
                              | Some t1, Some t2 -> begin match t1 with 
                                                    | TUnit -> "(func () " ^ compile_type type_map t2 ^ " {" ^ compile_exp single_channel type_map e' env ^ "})"
                                                    | _ -> "(func (" ^ v ^ " " ^ compile_type type_map t1 ^ ") " ^ compile_type type_map t2 ^ " {" ^ compile_exp single_channel type_map e' env ^ "})"
                                                    end
                              | _, _ -> assert false
                              end
  | Var v -> v
  | BOp (_, _, _) -> compile_bop single_channel type_map exp env
  | UOp (_, _) -> compile_uop single_channel type_map exp env
  (*Let expressions must be fully interpreted before being compiled, otherwise there is no way to simply define x in Go (x:=e1) and keep the let as a possible expression, usable anywhere in the code*)
  | Let (x, e1, e2) -> let nenv = Interpreter.StrMap.add x e1 env in 
                        let ne2 = Interpreter.eval nenv e2 in
                          compile_exp single_channel type_map ne2 nenv  (* x ^ " := " ^ compile_exp type_map e1 env ^ "\n" ^ compile_exp type_map e2 env *)
  | FunApp (e1, e2) -> begin match e2 with 
                       | UnitVal -> compile_exp single_channel type_map e1 env ^ "()"
                       | _ ->  compile_exp single_channel type_map e1 env ^ "(" ^ compile_exp single_channel type_map e2 env ^ ")"
                       end
  | Annot (e', _) -> compile_exp single_channel type_map e' env
  | Cond (cond, e1 ,e2) -> "if " ^ compile_exp single_channel type_map cond env ^ " {\n" ^ compile_exp single_channel type_map e1 env ^ "\n} else {\n" ^ compile_exp single_channel type_map e2 env ^ "\n}\n"
  | ProcExp (c, p, opt, ctxt) -> begin match opt with 
                                | Some st ->  begin match TypeTable.find_opt type_map st with
                                              | Some State (id, _) -> if List.length ctxt = 0 then 
                                                                      (*a process expression always compiles to a go function that receives its channel and other channels from context *)
                                                                        "func (" ^ c ^ " *" ^ id ^ "){\n" ^ compile_proc single_channel type_map p SeqMap.empty env ^ "}"
                                                                      else
                                                                        "func (" ^ c ^ " *" ^ id ^ ", " ^ compile_lin_ctxt_to_fun_args ctxt type_map ^ "){\n" ^ compile_proc single_channel type_map p SeqMap.empty env ^ "}"
                                              | None -> assert false
                                              end
                                | None -> assert false
                                end
  | ExecExp exp -> begin match exp with 
                   | ProcExp (c, _, opt, _) -> begin match opt with 
                                            | Some st -> begin match TypeTable.find_opt type_map st with
                                                         | Some State (id, _) ->  begin match single_channel with 
                                                                                  (*The final goroutine that receives in c is necessary to avoid deadlock since the c process will always end on a close (send(nil)) to be well typed *)
                                                                                  | true ->  "func main () {\n" ^ c ^ " := init" ^ id ^ "(make (chan interface{}))\n" ^ "go func () {\n" ^ c ^ ".Recv()\n}()\n" ^  compile_exp single_channel type_map exp env ^ "(" ^ c ^ ")\n}\n" 
                                                                                  | false -> "func main () {\n" ^ c ^ " := init" ^ id ^ "()\n" ^ "go func () {\n" ^ c ^ ".Recv()\n}()\n" ^  compile_exp single_channel type_map exp env ^ "(" ^ c ^ ")\n}\n"
                                                                                  end
                                                         | None -> assert false
                                                         end
                                            | None -> assert false
                                            end
                  (* Since we allow for an executable expression to be a function call, we must interpret it to obtain the resulting procExp which will be executed *)
                   | FunApp (_, _) -> compile_exp single_channel type_map (ExecExp (Interpreter.eval env exp)) env
                   | _ -> error (NonExecutableExpression exp)
                   end
(* This function is meant to be used in declaration compilation, which need a return before the expression value, since a declaration is a function 
   This can be refactored by calling the compile_exp function; instead of just being a copy of it *)
and compile_return_exp single_channel type_map exp env =
match exp with
| UnitVal -> "return struct{}{}"
| Num n -> "return " ^ string_of_int n
| Bool b -> "return " ^ string_of_bool b
| FunDef (v, ty, e', ret) -> begin match ty, ret with 
                            | Some t1, Some t2 -> begin match t1 with 
                                                  | TUnit -> "return func () " ^ compile_type type_map t2 ^ "{\n" ^ compile_return_exp single_channel type_map e' env ^ "}\n" 
                                                  | _ -> "return func (" ^ v ^ " " ^ compile_type type_map t1 ^ ") " ^ compile_type type_map t2 ^ "{\n" ^ compile_return_exp single_channel type_map e' env ^ "}\n" 
                                                  end
                            | _, _ -> assert false
                            end
| Var v -> "return " ^ v
| BOp (_, _, _) -> "return " ^ compile_bop single_channel type_map exp env
| UOp (_, _) -> "return " ^ compile_uop single_channel type_map exp env
| Let (x, e1, e2) -> let nenv = Interpreter.StrMap.add x e1 env in 
                      let ne2 = Interpreter.eval nenv e2 in
                        compile_return_exp single_channel type_map ne2 nenv
| FunApp (e1, _) -> "return " ^ compile_exp single_channel type_map e1 env (*TODO: Why is e2 not used!? *)
| Annot (e', _) -> compile_return_exp single_channel type_map e' env
| Cond (cond, e1 ,e2) -> "if " ^ compile_exp single_channel type_map cond env ^ " {\n" ^ compile_return_exp single_channel type_map e1 env ^ "\n} else {\n" ^ compile_return_exp single_channel type_map e2 env ^ "\n}\n"
| ProcExp (_, _, _, _) -> "return " ^ compile_exp single_channel type_map exp env
| ExecExp _ -> assert false (*This case is never reached because this function is always called for the expressions of declarations, which are never ExecExps*)
and compile_proc single_channel type_map p id_map env = 
  match p with
  | Send (d, exp, _, proc) -> let curr_id = curr_seq_id d id_map in 
                                  let (next_id, next_map) = next_seq_id d id_map in
                                       next_id ^ " := " ^ curr_id ^ ".Send(" ^ compile_exp single_channel type_map exp env ^ ")\n" ^  compile_proc single_channel type_map proc next_map env 
  | Recv (id, d, _, proc) -> let curr_id = curr_seq_id d id_map in
                                let (next_id, next_map) = next_seq_id d id_map in
                                   id ^ ", " ^ next_id ^ " := " ^ curr_id ^ ".Recv()\n" ^  compile_proc single_channel type_map proc next_map env
  | Close d -> curr_seq_id d id_map ^ ".Send(nil)\n"
  | Wait (d, proc) -> let curr_id = curr_seq_id d id_map in 
                        curr_id ^ ".Recv()\n" ^ compile_proc single_channel type_map proc id_map env
  | Fwd (opt, c, d) -> begin match opt with 
                       | Some sty -> "// FWD " ^ c ^ " " ^ d ^ " Start\n" ^ compile_fwd_stype sty c d type_map id_map [] ^ "// FWD " ^ c ^ " " ^ d ^ " End\n"
                       | None -> assert false
                       end 
  | Spawn (d, exp, opt, proc, args) -> begin match opt with 
                                      | Some st -> begin match TypeTable.find_opt type_map st with
                                                    | Some State (id, _) -> begin match single_channel with
                                                      	                    | true -> (* let curr_id = curr_seq_id d id_map in // I can use just d instead of curr_seq_id since the id_map is always empty, since there is no matching id for d in id_map *)
                                                                                      if List.length args = 0 then 
                                                                                        d ^ " := init" ^ id ^ "(" ^ "make(chan interface{})" ^ ")\n" ^ "go " ^ compile_exp single_channel type_map exp env ^ "(" ^ d ^ ")\n" ^ compile_proc single_channel type_map proc id_map env
                                                                                      else
                                                                                        d ^ " := init" ^ id ^ "(" ^ "make(chan interface{})" ^ ")\n" ^ "go " ^ compile_exp single_channel type_map exp env ^ "(" ^ d ^ ", " ^ compile_var_list_to_fun_args args id_map ^ ")\n" ^ compile_proc single_channel type_map proc id_map env 
                                                                            | false  -> (* let curr_id = curr_seq_id d id_map in // I can use just d instead of curr_seq_id since the id_map is always empty, since there is no matching id for d in id_map *)
                                                                                      if List.length args = 0 then 
                                                                                        d ^ " := init" ^ id ^ "()\n" ^ "go " ^ compile_exp single_channel type_map exp env ^ "(" ^ d ^ ")\n" ^ compile_proc single_channel type_map proc id_map env
                                                                                      else
                                                                                        d ^ " := init" ^ id ^ "()\n" ^ "go " ^ compile_exp single_channel type_map exp env ^ "(" ^ d ^ ", " ^ compile_var_list_to_fun_args args id_map ^ ")\n" ^ compile_proc single_channel type_map proc id_map env  
                                                                            end
                                                    | None -> assert false
                                                    end
                                      | None -> assert false
                                      end
  | Choice (d, l) -> let curr_id = curr_seq_id d id_map in 
                      "label := " ^ curr_id ^ ".Recv()\n" ^ "switch label {\n" ^ compile_choice_list single_channel type_map d l id_map env ^  "}\n"
  | Label (d, l, proc, opt) -> begin match opt with 
                               | Some st -> begin match TypeTable.find_opt type_map st with
                                            | Some State (id, _) -> let curr_id = curr_seq_id d id_map in
                                                                      let (next_id, next_map) = next_seq_id d id_map in
                                                                        curr_id ^ ".Send(" ^ "\"" ^ l ^ "\"" ^ ")\n" ^ (next_id ^ " := " ^ curr_id ^ ".ls[" ^ "\"" ^ l ^ "\"].(*" ^ id ^ ")\n") ^ compile_proc single_channel type_map proc next_map env
                                            | None -> assert false
                                            end
                               | None -> assert false
                               end
  | SendChan (d, e, _, proc) -> let curr_id = curr_seq_id d id_map in
                                    let (next_id, next_map) = next_seq_id d id_map in
                                      next_id ^ " := " ^ curr_id ^ ".Send(" ^ e ^ ")\n" ^  compile_proc single_channel type_map proc next_map env
  | RecvChan (id, d, opt, proc) -> let curr_id = curr_seq_id d id_map in
                                    let (next_id, next_map) = next_seq_id d id_map in
                                      begin match single_channel with 
                                      | true -> let (var_id, final_map) = next_seq_id id next_map in
                                                    begin match opt with 
                                                    | Some st -> begin match TypeTable.find_opt type_map st with 
                                                                | Some State (state_id, _) -> id ^ ", " ^ next_id ^ " := " ^ curr_id ^ ".Recv()\n" ^ 
                                                                                                var_id ^ " := " ^ id ^ ".(*" ^ state_id ^ ")\n" ^
                                                                                                compile_proc single_channel type_map proc final_map env
                                                                  | None -> assert false
                                                                  end
                                                    | None -> assert false  
                                                    end
                                      | false -> id ^ ", " ^ next_id ^ " := " ^ curr_id ^ ".Recv()\n" ^ compile_proc single_channel type_map proc next_map env
                                      end
  | Print (e, proc) -> "fmt.Printf(\"%v\\n\"," ^ compile_exp single_channel type_map e env ^  ")\n" ^ compile_proc single_channel type_map proc id_map env
  | If (e, p1, p2) -> "if " ^ compile_exp single_channel type_map e env ^ " {\n" ^ compile_proc single_channel type_map p1 id_map env ^ "} else {\n" ^ compile_proc single_channel type_map p2 (copy_id_map id_map) env ^ "}\n"
and compile_var_list_to_fun_args vars id_map = 
  match vars with 
  | [v] -> curr_seq_id v id_map 
  | v::tail -> curr_seq_id v id_map ^ ", " ^ compile_var_list_to_fun_args tail id_map
  | [] -> ""
(* Compile the appropriate behavior to redirect messages from one process to another and vice-versa; 
   the big challenge here is the recursion. A recursive fwd compiles to an infinite for cycle. *)
and compile_fwd_stype ?start_c:(start_c = "") ?start_d:(start_d = "") sty c d type_map id_map vars = 
  match sty with (* st is type of d *)
  | STSend (_, st) -> let curr_d = curr_seq_id d id_map in
                        let next_d, fst_map = next_seq_id d id_map in 
                          let curr_c = curr_seq_id c fst_map in
                            let next_c, snd_map = next_seq_id c fst_map in 
                              let aux_id = curr_c ^ curr_d in
                               let mid_d = (curr_c ^ "_"  ^ curr_d) in
                                begin match st with 
                                | STRec (v, _) -> begin match List.find_opt (fun el -> el = v) vars with
                                                  | Some _ -> aux_id ^ ", " ^ mid_d ^ " := " ^ curr_d ^ ".Recv()\n" ^
                                                              start_d ^ " = " ^ mid_d ^ "\n" ^
                                                              start_c ^ " = " ^ curr_c ^ ".Send(" ^ aux_id ^")\n"
                                                  | None -> aux_id ^ ", " ^ next_d ^ " := " ^ curr_d ^ ".Recv()\n" ^
                                                            next_c ^ " := " ^ curr_c ^ ".Send(" ^ aux_id ^")\n" ^
                                                            compile_fwd_stype ~start_c ~start_d st c d type_map snd_map vars 
                                                  end
                                | _ -> aux_id ^ ", " ^ next_d ^ " := " ^ curr_d ^ ".Recv()\n" ^
                                      next_c ^ " := " ^ curr_c ^ ".Send(" ^ aux_id ^")\n" ^
                                      compile_fwd_stype ~start_c ~start_d st c d type_map snd_map vars
                                end
  | STRecv (_, st) -> let curr_d = curr_seq_id d id_map in
                        let next_d, fst_map = next_seq_id d id_map in 
                          let curr_c = curr_seq_id c fst_map in
                            let next_c, snd_map = next_seq_id c fst_map in 
                              let aux_id = curr_d ^ curr_c in
                                let mid_c = (curr_c ^ "_"  ^ curr_d) in
                                begin match st with 
                                | STRec (v, _) -> begin match List.find_opt (fun el -> el = v) vars with
                                                  | Some _ -> aux_id ^ ", " ^ mid_c ^ " := " ^ curr_c ^ ".Recv()\n" ^
                                                              start_c ^ " = " ^ mid_c ^ "\n" ^
                                                              start_d ^ " = " ^ curr_d ^ ".Send(" ^ aux_id ^")\n" 
                                                  | None -> aux_id ^ ", " ^ next_c ^ " := " ^ curr_c ^ ".Recv()\n" ^
                                                            next_d ^ " := " ^ curr_d ^ ".Send(" ^ aux_id ^")\n" ^
                                                            compile_fwd_stype ~start_c ~start_d st c d type_map snd_map vars
                                                  end
                                | _ -> aux_id ^ ", " ^ next_c ^ " := " ^ curr_c ^ ".Recv()\n" ^
                                       next_d ^ " := " ^ curr_d ^ ".Send(" ^ aux_id ^")\n" ^
                                       compile_fwd_stype ~start_c ~start_d st c d type_map snd_map vars
                                end
  | STExtChoice l -> let curr_d = curr_seq_id d id_map in
                        let curr_c = curr_seq_id c id_map in
                            let label = curr_d ^ curr_c in
                              label ^ " := " ^ curr_c ^ ".Recv()\n" ^ (* receive label *)
                              curr_d ^ ".Send(" ^ label ^ ")\n" ^ (* propagate label *)
                              "switch " ^ label ^ " {\n" ^ compile_fwd_stype_list ~start_c ~start_d l c d type_map id_map vars ^ "}\n"
  | STIntChoice l -> let curr_d = curr_seq_id d id_map in
                      let curr_c = curr_seq_id c id_map in
                       let label = curr_c ^ curr_d in
                        label ^ " := " ^ curr_d ^ ".Recv()\n" ^ (* receive label *)
                        curr_c ^ ".Send(" ^ label ^ ")\n" ^ (* propagate label *)
                        "switch " ^ label ^ " {\n" ^ compile_fwd_stype_list ~start_c ~start_d l c d type_map id_map vars ^ "}\n"
  | STSendChan (_, st2) -> let curr_d = curr_seq_id d id_map in
                              let next_d, fst_map = next_seq_id d id_map in 
                                let curr_c = curr_seq_id c fst_map in
                                  let next_c, snd_map = next_seq_id c fst_map in 
                                    let aux_id = curr_c ^ curr_d in
                                      let mid_d = (curr_c ^ "_"  ^ curr_d) in
                                      begin match st2 with 
                                      | STRec (v, _) -> begin match List.find_opt (fun el -> el = v) vars with
                                                        | Some _ -> aux_id ^ ", " ^ mid_d ^ " := " ^ curr_d ^ ".Recv()\n" ^
                                                                    start_d ^ " = " ^ mid_d ^ "\n" ^
                                                                    start_c ^ " = " ^ curr_c ^ ".Send(" ^ aux_id ^ ")\n"
                                                        | None -> aux_id ^ ", " ^ next_d ^ " := " ^ curr_d ^ ".Recv()\n" ^
                                                                  next_c ^ " := " ^ curr_c ^ ".Send(" ^ aux_id ^ ")\n" ^
                                                                  compile_fwd_stype ~start_c ~start_d st2 c d type_map snd_map vars
                                                        end
                                      | _ ->  aux_id ^ ", " ^ next_d ^ " := " ^ curr_d ^ ".Recv()\n" ^
                                              next_c ^ " := " ^ curr_c ^ ".Send(" ^ aux_id ^ ")\n" ^
                                              compile_fwd_stype ~start_c ~start_d st2 c d type_map snd_map vars
                                      end
  | STRecvChan (_, st2) -> let curr_d = curr_seq_id d id_map in
                              let next_d, fst_map = next_seq_id d id_map in 
                                let curr_c = curr_seq_id c fst_map in
                                  let next_c, snd_map = next_seq_id c fst_map in 
                                    let aux_id = curr_d ^ curr_c in
                                      let mid_c = (curr_c ^ "_"  ^ curr_d) in
                                      begin match st2 with 
                                      | STRec (v, _) -> begin match List.find_opt (fun el -> el = v) vars with
                                                        | Some _ -> aux_id ^ ", " ^ mid_c ^ " := " ^ curr_c ^ ".Recv()\n" ^
                                                                    start_c ^ " = " ^ mid_c ^ "\n" ^ 
                                                                    start_d ^ " = " ^ curr_d ^ ".Send(" ^ aux_id ^ ")\n"
                                                        | None -> aux_id ^ ", " ^ next_c ^ " := " ^ curr_c ^ ".Recv()\n" ^
                                                                  next_d ^ " := " ^ curr_d ^ ".Send(" ^ aux_id ^ ")\n" ^
                                                                  compile_fwd_stype ~start_c ~start_d st2 c d type_map snd_map vars
                                                        end
                                      | _ -> aux_id ^ ", " ^ next_c ^ " := " ^ curr_c ^ ".Recv()\n" ^
                                             next_d ^ " := " ^ curr_d ^ ".Send(" ^ aux_id ^ ")\n" ^
                                             compile_fwd_stype ~start_c ~start_d st2 c d type_map snd_map vars
                                      end
  | STVar _ -> ""
  | STRec (v, _) -> begin match List.find_opt (fun el -> el = v) vars with
                      | Some _ -> ""
                      | None -> let curr_c = curr_seq_id c id_map in 
                                  let curr_d = curr_seq_id d id_map in 
                                    "for {\n" ^ compile_fwd_stype ~start_c:curr_c ~start_d:curr_d (unfold sty) c d type_map id_map (v::vars) ^ "}\n" 
                     end
  | STEnd -> let curr_d = curr_seq_id d id_map in
              let curr_c = curr_seq_id c id_map in
                curr_d ^ ".Recv()\n" ^
                curr_c ^ ".Send(nil)\n" ^
                "return\n"
  | STUVar _ -> assert false
(* The function that compiles a list of label-stype pairs. The challenge here is maintaining consistent id usage across cases.
   This function can be simplified by simply using hard copy maps (from copy_id_map), 
   instead of keeping track of initial ids and restoring the maps to their initial states.
   *)
and compile_fwd_stype_list ?start_c:(start_c = "") ?start_d:(start_d = "") l c d type_map id_map vars =
  match l with 
  | (str, sty)::tail -> begin match TypeTable.find_opt type_map sty with 
                        | Some State (state, _) -> begin match SeqMap.find_opt d id_map with (* record starting state of the id_map for the next case *)
                                                   | Some d_ref -> begin match SeqMap.find_opt c id_map with 
                                                                    | Some c_ref -> let init_d = !d_ref in
                                                                                      let init_c = !c_ref in
                                                                                        let curr_d = curr_seq_id d id_map in
                                                                                          let next_d, fst_map = next_seq_id d id_map in 
                                                                                            let curr_c = curr_seq_id c fst_map in
                                                                                              let next_c, snd_map = next_seq_id c fst_map in
                                                                                                let continuation = compile_fwd_stype ~start_c ~start_d sty c d type_map snd_map vars in
                                                                                                  if continuation = "" then compile_fwd_stype_list ~start_c ~start_d tail c d type_map (SeqMap.add c (ref init_c) (SeqMap.add d (ref init_d) id_map)) vars
                                                                                                  else  "case \"" ^ str ^ "\":\n" ^ 
                                                                                                        next_d ^ " := " ^ curr_d ^ ".ls[\"" ^ str ^ "\"].(*" ^ state ^ ")\n" ^ 
                                                                                                        next_c ^ " := " ^ curr_c ^ ".ls[\"" ^ str ^ "\"].(*" ^ state ^ ")\n" ^ 
                                                                                                        continuation ^ compile_fwd_stype_list ~start_c ~start_d tail c d type_map (SeqMap.add c (ref init_c) (SeqMap.add d (ref init_d) id_map)) vars
                                                                      | None -> let init_d = !d_ref in
                                                                                    let curr_d = curr_seq_id d id_map in
                                                                                      let next_d, fst_map = next_seq_id d id_map in 
                                                                                        let curr_c = curr_seq_id c fst_map in
                                                                                          let next_c, snd_map = next_seq_id c fst_map in
                                                                                            let continuation = compile_fwd_stype ~start_c ~start_d sty c d type_map snd_map vars in
                                                                                              if continuation = "" then compile_fwd_stype_list ~start_c ~start_d tail c d type_map (SeqMap.remove c (SeqMap.add d (ref init_d) id_map)) vars
                                                                                              else  "case \"" ^ str ^ "\":\n" ^ 
                                                                                                    next_d ^ " := " ^ curr_d ^ ".ls[\"" ^ str ^ "\"].(*" ^ state ^ ")\n" ^ 
                                                                                                    next_c ^ " := " ^ curr_c ^ ".ls[\"" ^ str ^ "\"].(*" ^ state ^ ")\n" ^ 
                                                                                                    continuation ^ compile_fwd_stype_list ~start_c ~start_d tail c d type_map (SeqMap.remove c (SeqMap.add d (ref init_d) id_map)) vars
                                                                      end
                                                    | None -> begin match SeqMap.find_opt c id_map with 
                                                              | Some c_ref -> let curr_d = curr_seq_id d id_map in
                                                                                let init_c = !c_ref in
                                                                                  let next_d, fst_map = next_seq_id d id_map in 
                                                                                    let curr_c = curr_seq_id c fst_map in
                                                                                      let next_c, snd_map = next_seq_id c fst_map in
                                                                                        let continuation = compile_fwd_stype ~start_c ~start_d sty c d type_map snd_map vars in 
                                                                                          if continuation = "" then compile_fwd_stype_list ~start_c ~start_d tail c d type_map (SeqMap.add c (ref init_c) (SeqMap.remove d id_map)) vars 
                                                                                          else  "case \"" ^ str ^ "\":\n" ^ 
                                                                                                next_d ^ " := " ^ curr_d ^ ".ls[\"" ^ str ^ "\"].(*" ^ state ^ ")\n" ^ 
                                                                                                next_c ^ " := " ^ curr_c ^ ".ls[\"" ^ str ^ "\"].(*" ^ state ^ ")\n" ^ 
                                                                                                continuation ^ compile_fwd_stype_list ~start_c ~start_d tail c d type_map (SeqMap.add c (ref init_c) (SeqMap.remove d id_map)) vars
                                                              | None -> let curr_d = curr_seq_id d id_map in
                                                                          let next_d, fst_map = next_seq_id d id_map in 
                                                                            let curr_c = curr_seq_id c fst_map in
                                                                              let next_c, snd_map = next_seq_id c fst_map in
                                                                                let continuation  = compile_fwd_stype ~start_c ~start_d sty c d type_map snd_map vars in 
                                                                                  if continuation = "" then compile_fwd_stype_list ~start_c ~start_d tail c d type_map (SeqMap.remove c (SeqMap.remove d id_map)) vars
                                                                                  else  "case \"" ^ str ^ "\":\n" ^ 
                                                                                        next_d ^ " := " ^ curr_d ^ ".ls[\"" ^ str ^ "\"].(*" ^ state ^ ")\n" ^ 
                                                                                        next_c ^ " := " ^ curr_c ^ ".ls[\"" ^ str ^ "\"].(*" ^ state ^ ")\n" ^ 
                                                                                        continuation ^ compile_fwd_stype_list ~start_c ~start_d tail c d type_map (SeqMap.remove c (SeqMap.remove d id_map)) vars
                                                              end
                                                    end
                        | None -> assert false
                        end
  | [] -> ""
and compile_choice_list single_channel type_map d ls id_map env = 
  match ls with
  | (l, (proc, opt))::tail -> begin match opt with 
                              | Some st -> begin match TypeTable.find_opt type_map st with
                                           | Some State (id, _) -> begin match SeqMap.find_opt d id_map with (* record starting state of the id_map for the next case *)
                                                                   | Some start_ref -> let start_num = !start_ref in
                                                                                        let curr_id = curr_seq_id d id_map in
                                                                                          let (next_id, next_map) = next_seq_id d id_map in
                                                                                            "case \"" ^ l ^ "\" :\n" ^ next_id ^ " := " ^ curr_id ^ ".ls[" ^ "\"" ^ l ^ "\"].(*" ^ id ^ ")\n" ^ 
                                                                                            compile_proc single_channel type_map proc (copy_id_map next_map) env ^ 
                                                                                            compile_choice_list single_channel type_map d tail (SeqMap.add d (ref start_num) id_map) env (* restore starting state of the id_map for the next case *)
                                                                    | None -> let curr_id = curr_seq_id d id_map in
                                                                                let (next_id, next_map) = next_seq_id d id_map in
                                                                                  "case \"" ^ l ^ "\" :\n" ^ next_id ^ " := " ^ curr_id ^ ".ls[" ^ "\"" ^ l ^ "\"].(*" ^ id ^ ")\n" ^ 
                                                                                  compile_proc single_channel type_map proc (copy_id_map next_map) env ^ 
                                                                                  compile_choice_list single_channel type_map d tail (SeqMap.remove d id_map) env (* restore starting state of the id_map for the next case *) 
                                                                    end
                                           | None -> assert false
                                           end
                              | None -> assert false
                              end
  | [] -> ""
and compile_bop single_channel type_map e env =
match e with 
| BOp (bop, e1, e2) -> begin match bop with
                        | Mul -> "(" ^ compile_exp single_channel type_map e1 env ^ " * " ^ compile_exp single_channel type_map e2 env ^ ")"
                        | Div -> "(" ^ compile_exp single_channel type_map e1 env ^ " / " ^ compile_exp single_channel type_map e2 env ^ ")"
                        | Add -> "(" ^ compile_exp single_channel type_map e1 env ^ " + " ^ compile_exp single_channel type_map e2 env ^ ")"
                        | Sub -> "(" ^ compile_exp single_channel type_map e1 env ^ " - " ^ compile_exp single_channel type_map e2 env ^ ")"
                        | And -> "(" ^ compile_exp single_channel type_map e1 env ^ " && " ^ compile_exp single_channel type_map e2 env ^ ")"
                        | Or -> "(" ^ compile_exp single_channel type_map e1 env ^ " || " ^ compile_exp single_channel type_map e2 env ^ ")"
                        | Lesser -> "(" ^ compile_exp single_channel type_map e1 env ^ " < " ^ compile_exp single_channel type_map e2 env ^ ")"
                        | Greater -> "(" ^ compile_exp single_channel type_map e1 env ^ " > " ^ compile_exp single_channel type_map e2 env ^ ")"
                        | Equals -> "(" ^ compile_exp single_channel type_map e1 env ^ " == " ^ compile_exp single_channel type_map e2 env ^ ")"
                        end
| _ -> assert false
and compile_uop single_channel type_map e env =  
match e with 
| UOp (uop, e') -> begin match uop with
                    | Neg -> "(" ^ "-" ^ compile_exp single_channel type_map e' env ^ ")"
                    | Not -> "(" ^ "!" ^ compile_exp single_channel type_map e' env ^ ")"
                    end
| _ -> assert false

let compile_decl single_channel map decl env = 
  match decl with
  | Decl (x, _, exp) -> begin match exp with 
                         | FunDef (v, Some arg, body, Some ret) -> begin match arg with 
                                                                  | TUnit -> ("func " ^ x ^ "() " ^ compile_type map ret ^ " {\n " ^ (compile_return_exp single_channel map body env) ^ "}\n", Interpreter.StrMap.add x exp env)
                                                                  | _ -> ("func " ^ x ^ "(" ^ v ^ " " ^ compile_type map arg ^ ") " ^ compile_type map ret ^ " {\n " ^ (compile_return_exp single_channel map body env) ^ "}\n", Interpreter.StrMap.add x exp env)
                                                                  end
                         | _ -> "", env (* Case of custom type declaration; has no effect on compilation result *)
                         end

(* Entry function *)
let compile_prog prog single_channel filename = 
  match prog with 
  | Prog (ldecls, e) -> (* Preamble creation *)
                        let stype_state_map = List.fold_left (fun map decl -> match decl with Decl (_, _, exp) -> make_state_trees_from_exp map exp) (TypeTable.create 50) ldecls in
                          let final_stype_state_map = make_state_trees_from_exp stype_state_map e in

                          let state_list = ref [] in TypeTable.iter (fun _ state -> state_list := state::!state_list) final_stype_state_map; 
                          
                          let preamble = ref "" in

                          begin match single_channel with 
                          | true -> preamble := compile_from_state_list !state_list final_stype_state_map compile_state_single_channel
                          | false -> preamble := compile_from_state_list !state_list final_stype_state_map compile_state_multi_channel
                          end;

                        
                        (* Declaration list compilation *)    
                        let compiled_decls, env = List.fold_left (fun (str, env) decl -> 
                                                              let dec_str, nenv = compile_decl single_channel final_stype_state_map decl env in
                                                                (str ^ dec_str, nenv)) ("", Interpreter.StrMap.empty) ldecls in  
                         
                        (* Debug print *)
                        TypeTable.iter (fun st (State (id, _)) ->  print_endline @@ Printer.string_from_stype st ^ " " ^ id) final_stype_state_map;

                        (* Put it all together *)
                        let main = compile_exp single_channel final_stype_state_map e env in
                          let oc = open_out filename in
                            Printf.fprintf oc "%s" ("package main\n" ^ "import (\"fmt\"\n\"sync\")\n" ^ !preamble ^ compiled_decls ^ main); (* TODO add way to judge if there is a print in the program; otherwise the unused import will prevent execution; the same goes for sync *)
                            close_out oc;
