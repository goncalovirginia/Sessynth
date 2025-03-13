open Syntax
open Printer

module StrMap = Map.Make (String)

(* Types of message payloads that will be sent through channels *)
type msg =
  | MsgLabel of var
  | MsgExp of exp
  | MsgChan of msg Event.channel

(* Get an exp eval function based on the binary operator *)
let [@warning "-8"] eval_bop op =
  match op with 
  | Mul -> fun (Num n1) (Num n2) -> Num (n1 * n2)
  | Div -> fun (Num n1) (Num n2) -> Num (n1 / n2)
  | Add-> fun (Num n1) (Num n2) -> Num (n1 + n2)
  | Sub ->fun (Num n1) (Num n2) -> Num (n1 - n2)
  | And -> fun (Bool b1) (Bool b2) -> Bool (b1 && b2)
  | Or -> fun (Bool b1) (Bool b2) -> Bool (b1 || b2)
  | Lesser -> fun (Num n1) (Num n2) -> Bool (n1 < n2)
  | Greater-> fun (Num n1) (Num n2) -> Bool (n1 > n2)
  | Equals -> fun (n1) (n2) -> Bool (n1 = n2)

(* Get an exp eval function based on the unary operator *)  
let [@warning "-8"] eval_uop op = 
  match op with
  | Neg -> fun (Num n) -> Num (-n) 
  | Not -> fun (Bool b) -> Bool (not b)

(* Replace x with value of e1 in expression e2 *)
let rec subst x e1 e2 = 
  match e2 with 
  | UnitVal
  | Num (_)  
  | Bool (_) -> e2
  | Var (y) -> if x = y then e1 else e2
  | FunDef (y, opt, e, ret_ty) -> if x == y then e2 else FunDef (y, opt, subst x e1 e, ret_ty)
  | BOp (bop, e1', e2') -> BOp (bop, (subst x e1 e1'), (subst x e1 e2'))
  | UOp (uop, e) -> UOp (uop, (subst x e1 e))
  | Let (y, e1', e2') -> if x != y then Let (y, (subst x e1 e1'), (subst x e1 e2')) else Let (y, (subst x e1 e1'), e2') 
  | FunApp (e, e') -> FunApp ((subst x e1 e), (subst x e1 e'))
  | Annot (e, t) -> Annot ((subst x e1 e), t)
  | Cond (cond, e1', e2') -> Cond (subst x e1 cond, subst x e1 e1', subst x e1 e2')
  | ProcExp (c, proc, opt, ctxt) -> ProcExp (c, subst_proc x e1 proc, opt, ctxt)
  | ExecExp exp -> subst x e1 exp
and subst_proc x e p =
  match p with 
  | Close _
  | Fwd (_, _, _) -> p
  | Send (c, e', o, p') -> Send (c, (subst x e e'), o, (subst_proc x e p'))
  | Recv (v, c, opt, p') -> Recv (v, c, opt, (subst_proc x e p'))
  | Wait (c, p') -> Wait (c, (subst_proc x e p'))
  | Spawn (c, e', opt, p, args) -> Spawn (c, (subst x e e'), opt, (subst_proc x e p), args)
  | Choice (c, ls) -> Choice (c, (subst_choice_proc x e ls))
  | Label (c, l, p', o) -> Label (c, l, (subst_proc x e p'), o)
  | SendChan (c, v, o, p') -> SendChan (c, v, o, (subst_proc x e p'))
  | RecvChan (v, c, opt, p') -> RecvChan (v, c, opt, (subst_proc x e  p'))
  | Print (e', _) -> Print ((subst x e e'), (subst_proc x e p))
  | If (e', p1, p2) -> If ((subst x e e'), (subst_proc x e p1), (subst_proc x e p2))
and subst_choice_proc x e ls =
  match ls with 
  | (l, (p, o))::rest -> (l, ((subst_proc x e p), o))::(subst_choice_proc x e rest)
  | [] -> []

let rec eval env e =
  match e with
  | UnitVal
  | Num (_) 
  | Bool (_) 
  | FunDef (_ ,_, _, _) -> e
  | Var v -> StrMap.find v env
  | BOp (bop, e1, e2) -> (eval_bop bop) (eval env e1) (eval env e2)
  | UOp (uop, e') -> (eval_uop uop)  (eval env e')
  | Let (x, e1, e2) -> let e1' = eval env e1 in eval (StrMap.add x e1' env) (subst x e1' e2)
  | FunApp (e1, e2) -> begin match eval env e1 with 
                       | FunDef (x, _, body, _) -> eval env (subst x e2 body)                       
                       | _ -> assert false (* should never happen if well typed *)
                       end 
  | Annot (e', _) -> eval env e'
  | Cond (cond, e1 ,e2) -> begin match eval env cond with
                           | Bool b -> if b then eval env e1 else eval env e2
                           | _ -> assert false (* should never happen if well typed *)
                           end
  | ProcExp (_, _, _, _) -> e  
  | ExecExp exp -> begin match eval env exp with
                   | ProcExp (cname, proc, _, _) -> exec StrMap.empty env StrMap.empty proc cname (Event.new_channel ()); e
                   | _ -> assert false (* should never happen if well typed *)
                   end
and exec lin_ctxt env rec_env proc cname channel = 
  match proc with 
  | Send (c', e, _, p) -> if cname = c' then                      
                            let _ = Event.sync (Event.send channel (MsgExp (eval env e))) in 
                              exec lin_ctxt env rec_env p cname channel
                          else 
                            let chan = StrMap.find c' lin_ctxt in
                              let _ = Event.sync (Event.send chan (MsgExp (eval env e))) in
                                exec lin_ctxt env rec_env p cname channel
  | Recv (v, c', _, p) -> if cname = c' then
                            begin match Event.sync (Event.receive channel) with
                            | MsgExp value -> exec lin_ctxt (StrMap.add v value env) rec_env p cname channel
                            | _ -> assert false (* should never happen if well typed *)
                            end
                          else 
                            let chan = StrMap.find c' lin_ctxt in 
                              begin match Event.sync (Event.receive chan) with
                              | MsgExp value -> exec lin_ctxt (StrMap.add v value env) rec_env p cname channel
                              | _ -> assert false (* should never happen if well typed *)
                              end
  | Close _ -> ()(*Event.sync (Event.send channel MsgEnd)*)
  | Wait (_, _) -> ()(*begin match Event.sync (Event.receive channel) with
                    | MsgEnd -> ()
                    | _ -> assert false (* should never happen if well typed *)
                    end*)
  | Fwd (op, c', d) -> begin match op with
                       | Some st -> begin match st with 
                                     | STSend (t, st') -> let illegal_id = "%" in 
                                                            let unfold_proc = Recv (illegal_id, d, Some t, Send (c', Var illegal_id, Some t, Fwd (Some st', c', d))) in 
                                                              exec lin_ctxt env rec_env unfold_proc cname channel (* recv in x, send in y *)
                                     | STRecv (t, st') -> let illegal_id = "%" in 
                                                            let unfold_proc = Recv (illegal_id, c', Some t, Send (d, Var illegal_id, Some t, Fwd (Some st', c', d))) in 
                                                              exec lin_ctxt env rec_env unfold_proc cname channel (* recv in y, send in x *)
                                     | STEnd -> let unfold_proc = Wait (d, Close c') in 
                                                  exec lin_ctxt env rec_env unfold_proc cname channel (* wait on x, close y *)
                                     | STExtChoice l -> let fold_fun = fun new_pair_list (label, label_type) -> (label, (Label (d, label, Fwd (Some label_type, c', d), Some label_type), Some label_type))::new_pair_list in
                                                          let unfold_proc = Choice (c', List.fold_left fold_fun [] l) in
                                                            exec lin_ctxt env rec_env unfold_proc cname channel (* choice on y, label on x *)
                                    | STIntChoice l -> let fold_fun = fun new_pair_list (label, label_type) -> (label, (Label (c', label, Fwd (Some label_type, c', d), Some label_type), Some label_type))::new_pair_list in 
                                                          let unfold_proc = Choice (d, List.fold_left fold_fun [] l) in 
                                                            exec lin_ctxt env rec_env unfold_proc cname channel (* choice on x, label on y*)
                                     | STSendChan (sent_st, st') -> let illegal_id = "%" in 
                                                                      let unfold_proc = RecvChan (illegal_id, d, Some sent_st, SendChan (c', illegal_id, Some sent_st, Fwd (Some st', c', d))) in 
                                                                        exec lin_ctxt env rec_env unfold_proc cname channel (* recv in x, send in y *)
                                     | STRecvChan (recvd_st, st') -> let illegal_id = "%" in 
                                                                      let unfold_proc = RecvChan (illegal_id, c', Some recvd_st, SendChan (d, illegal_id, Some recvd_st, Fwd (Some st', c', d))) in 
                                                                        exec lin_ctxt env rec_env unfold_proc cname channel (* recv in y, send in x *)
                                     | STRec (x, st') -> let new_rec_env = StrMap.add x st' rec_env in
                                                           exec lin_ctxt env new_rec_env (Fwd (Some st', c', d)) cname channel
                                     | STVar x -> let st' = StrMap.find x rec_env in 
                                                    exec lin_ctxt env rec_env (Fwd (Some st', c', d)) cname channel
                                     | STUVar _ -> assert false
                                    end
                       | None -> assert false (* should never happen if well typed *)
                       end
  (* 
    fwd_A x y
    fwd_{A -o B} x y = y(a).x<a>.fwd_B x y
    fwd_{A * B} x y = x(a).y<a>.fwd_B x y
    fwd_{A & B} x y = case y of inl => x.inl ; fwd_A x y    | inr => x.inr ; fwd_B x y
    fwd_{A + B} x y = case x of inl => y.inl ; fwd_A x y |   inr => y.inr ; fwd_B x y
    fwd_1 x y = wait x ; close y
  *)
  | SendChan (c', v, _, p) -> if cname = c' then
                                let _ = Event.sync (Event.send channel (MsgChan (StrMap.find v lin_ctxt))) in
                                  exec lin_ctxt env rec_env p cname channel
                              else 
                                let chan = StrMap.find c' lin_ctxt in
                                  let _ = Event.sync (Event.send chan (MsgChan (StrMap.find v lin_ctxt))) in
                                    exec lin_ctxt env rec_env p cname channel
  | RecvChan (v, c', _, p) -> if cname = c' then
                                begin match Event.sync (Event.receive channel) with
                                | MsgChan recvd -> exec (StrMap.add v recvd lin_ctxt) env rec_env p cname channel
                                | _ -> assert false (* should never happen if well typed *)
                                end
                               else 
                                let chan = StrMap.find c' lin_ctxt in 
                                  begin match Event.sync (Event.receive chan) with
                                  | MsgChan recvd -> exec (StrMap.add v recvd lin_ctxt) env rec_env p cname channel
                                  | _ -> assert false (* should never happen if well typed *)
                                  end
  | Spawn (c', e, _, p, _) -> let new_chan = (Event.new_channel ()) in
                                begin match (eval env e) with
                                | ProcExp (cname', p', _, _) -> let _ = Thread.create (fun _ -> exec StrMap.empty env StrMap.empty p' cname' new_chan) () in
                                                            exec (StrMap.add c' new_chan lin_ctxt) env rec_env p cname channel
                                | _ -> assert false (* should never happen if well typed *)
                                end 
  | Choice (c', ls) -> if cname = c' then (* offer choice *)
                        (* receive label that chooses receiving process behavior; proceed as matching process *)
                        begin match Event.sync (Event.receive channel) with
                        | MsgLabel label -> begin match List.assoc_opt label ls with
                                            | Some (p, _) -> exec lin_ctxt env rec_env p cname channel (* proceed as the process that matches the received label *)
                                            | None -> assert false (* should never happen if well typed *)
                                            end
                        | _ -> assert false (* should never happen if well typed *)
                        end
                       else (* have an internal choice (sum type) in ctxt *)                        
                        let chan = StrMap.find c' lin_ctxt in 
                          (* receive label that tells what behavior the sending process will follow *)
                          begin match Event.sync (Event.receive chan) with
                          | MsgLabel label -> begin match List.assoc_opt label ls with
                                              | Some (p, _) -> exec lin_ctxt env rec_env p cname channel (* proceed as the process that matches the received label *)
                                              | None -> assert false (* should never happen if well typed *)
                                              end
                          | _ -> assert false (* should never happen if well typed *)
                          end
  | Label (c', l, p, _) -> if cname = c' then
                          (* decide own behavior; send label and proceed *)
                          let _ = Event.sync (Event.send channel (MsgLabel l)) in
                            exec lin_ctxt env rec_env p cname channel
                        else  
                          (* choose behavior of process in ctxt; send label and proceed *)
                          let chan = StrMap.find c' lin_ctxt in
                            let _ = Event.sync (Event.send chan (MsgLabel l)) in
                              exec lin_ctxt env rec_env p cname channel
  | Print (e, p) -> print_endline @@ string_from_exp (eval env e); exec lin_ctxt env rec_env p cname channel
  | If (e, p1, p2) -> begin match eval env e with
                      | Bool b -> if b then exec lin_ctxt env rec_env p1 cname channel else exec lin_ctxt env rec_env p2 cname channel
                      | _ -> assert false (* should never happen if well typed *)
                      end
let eval_decl env decl = 
  match decl with
  | Decl (x, _, exp) -> (x, eval (StrMap.add x exp env) exp)

(* Populates environment and evaluates final expression *) 
let eval_program prog = 
  match prog with 
  | Prog (ldecls, e) -> 
    let env = List.fold_left (fun acc decl -> let (key, data) = (eval_decl acc decl) in StrMap.add key data acc) StrMap.empty ldecls in 
      eval env e
