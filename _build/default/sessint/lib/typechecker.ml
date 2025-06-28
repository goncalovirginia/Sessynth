open Syntax
open Printer

(** Substitute label x for stype t in stype s *)
let rec subst_stype t x s =
  match s with
  | STSend (ty, sty) -> STSend (ty, subst_stype t x sty)
  | STRecv (ty, sty) -> STRecv (ty, subst_stype t x sty)
  | STEnd -> STEnd
  | STExtChoice l -> STExtChoice (subst_stype_list t x l)
  | STIntChoice l -> STIntChoice (subst_stype_list t x l)
  | STSendChan (st1, st2) -> STSendChan (subst_stype t x st1, subst_stype t x st2)
  | STRecvChan (st1, st2) -> STRecvChan (subst_stype t x st1, subst_stype t x st2)
  | STVar v -> if x = v then t else s
  | STRec (v, _) -> if x = v then s else STRec (v, subst_stype t x s)
  | STUVar _ -> s
  | STMultiSend _ | STMultiRecv _ -> assert false (*not possible in typechecker phase*)

(** Substitute label x for stype t in type sty *)
and subst_stype_list t x l =
  match l with
  | (v, sty) :: ll -> [ (v, subst_stype t x sty) ] @ subst_stype_list t x ll
  | [] -> []

(** Unfold recursive type only once
    Does nothing if the session types is not recursive*)
let unfold st = match st with STRec (x, t) -> subst_stype (STRec (x, t)) x t | _ -> st

(** Subtyping function. Return true if st1 subtype of st2; ctxt is a list of tentative instances of the subtyping relation *)
let rec subtyping ctxt st1 st2 =
  (* Look up the stype pair in the ctxt *)
  if List.mem (st1, st2) ctxt then true
  else
    match (st1, st2) with
    | STEnd, STEnd -> true
    | STRec (x, t), u ->
        subtyping ([ (STRec (x, t), u) ] @ ctxt) (unfold (STRec (x, t))) u
    | t, STRec (x, u) ->
        subtyping ([ (t, STRec (x, u)) ] @ ctxt) t (unfold (STRec (x, u)))
    | STRecv (tl, t), STRecv (ul, u) -> ty_eq tl ul && subtyping ctxt t u
    | STSend (tl, t), STSend (ul, u) -> ty_eq ul tl && subtyping ctxt t u
    | STExtChoice ls, STExtChoice lt ->
        let map_fun (l, s) =
          match List.assoc_opt l lt with Some t -> subtyping ctxt s t | None -> false
        in
        List.for_all map_fun ls
    | STIntChoice ls, STIntChoice lt ->
        let map_fun (l, t) =
          match List.assoc_opt l ls with Some s -> subtyping ctxt s t | None -> false
        in
        List.for_all map_fun lt
    | STRecvChan (tl, t), STRecvChan (ul, u) -> subtyping ctxt tl ul && subtyping ctxt t u
    | STSendChan (tl, t), STSendChan (ul, u) -> subtyping ctxt ul tl && subtyping ctxt t u
    | STMultiRecv (tl, t), STMultiRecv (ul, u) | STMultiSend (tl, t), STMultiSend (ul, u)
      ->
        multi_eq tl ul && subtyping ctxt t u
    | _ -> false

and multi_eq l1 l2 =
  match (l1, l2) with
  | t1 :: tail1, t2 :: tail2 -> ty_eq t1 t2 && multi_eq tail1 tail2
  | [], [] -> true
  | _ -> false

(** Base type equality (Tprocs are equal if the inner stypes are equal and their contexts are the same) *)
and ty_eq t1 t2 =
  match (t1, t2) with
  | TProc (p1, ctxt1), TProc (p2, ctxt2) ->
      match_lin_ctxt ctxt1 ctxt2 && (subtyping [] p1 p2 || subtyping [] p2 p1)
  | _ -> t1 = t2

and match_lin_ctxt ctxt1 ctxt2 =
  if List.length ctxt1 = List.length ctxt2 then
    let matching_function (c, st) =
      match List.assoc_opt c ctxt2 with
      | Some st' -> subtyping [] st st' || subtyping [] st' st
      | None -> false
    in
    List.for_all matching_function ctxt1
  else false

(** 
@param lin_ctxt a linear context (delta), which is used to keep track of available channels if checking/synthesizing the spawning of a process
@param env a functional environment (psi)
@param e the functional value to be typed
@param t type to check against
@param used_vars a list of used identifiers(variables), which do not impact the process of checking/synthesizing but are required for a future compilation step
@return a tuple containing the evaluated functional value (which is annotated with additional type information), the checked/synthesized type, and the identifiers used in the typing procedure.

This is the checking function.
Passing a lin_ctxt is necessary in the case that this check was called as a spawn, which receives a lin_ctxt.
The used_vars params keeps track of all variables that are actually used after being defined; this is for compilation purposes only, and has no effect on type checking.
Note that when returning, we rebuild e, with updated subexpressions and processes; this is necessary, since those updated nodes may now be annotated with information required for compilation.
*)
let rec check lin_ctxt env e t used_vars =
  match e with
  | Let (x, e1, e2) -> (
      let e1', t1, e1_vars = synth lin_ctxt env e1 used_vars in
      let e2', t2, e2_vars = check lin_ctxt ((x, t1) :: env) e2 t e1_vars in
      match List.find_opt (fun a -> a = x) e2_vars with
      | None -> (Let ("_", e1', e2'), t2, e2_vars)
      | Some _ -> (Let (x, e1', e2'), t2, e2_vars))
  | FunDef (x, _, e, _) -> (
      match t with
      | TFun (t1, t2) -> (
          match check lin_ctxt ((x, t1) :: env) e t2 used_vars with
          | e', _, vars -> (FunDef (x, Some t1, e', Some t2), t, vars))
      | _ -> error (NotFunctionType t))
  | ProcExp (chan, proc, _, _) -> (
      match t with
      | TProc (st, ctxt) ->
          let out_ctxt, p', st', vars = synth_proc ctxt env proc chan used_vars in
          if out_ctxt = [] then
            if subtyping [] st st' || subtyping [] st' st then
              (ProcExp (chan, p', Some st, ctxt), t, vars)
              (* st and st' are different here, should pass st, since that contains all labels in case of internal choice *)
            else error (NonMatchingSTypes (st, st'))
          else error NonEmptyLinearContext
      | _ -> error (NotProcessType t))
  | _ ->
      let e', t', vars = synth lin_ctxt env e used_vars in
      if ty_eq t' t then (e', t', vars) else error (UnexpectedType (t', t))

(** synthesizing a type from a functional value  
@param lin_ctxt a linear context (delta), which is used to keep track of available channels if checking/synthesizing the spawning of a process
@param env a functional environment (psi)
@param e the functional value to be typed
@param used_vars a list of used identifiers(variables), which do not impact the process of checking/synthesizing but are required for a future compilation step
@return a tuple containing the evaluated functional value (which is annotated with additional type information), the checked/synthesized type, and the identifiers used in the typing procedure.*)
(* NEW - correção de bug com variáveis que não apareciam - passar used_vars No UnitVal Num Bool e Var *)
and synth lin_ctxt env e used_vars =
  match e with
  | UnitVal -> (e, TUnit, used_vars)
  | Num _ -> (e, TNum, used_vars)
  | Bool _ -> (e, TBool, used_vars)
  | Var v ->
      if List.mem_assoc v env then (e, List.assoc v env, used_vars @ [ v ])
      else error (NoSuchArg v)
  | FunDef (x, Some t, b, _) ->
      let b', ret_ty, vars = synth lin_ctxt ((x, t) :: env) b used_vars in
      (FunDef (x, Some t, b', Some ret_ty), TFun (t, ret_ty), vars)
  | FunDef (_, None, _, _) -> error CannotInferType
  | BOp (_, _, _) -> synth_bop lin_ctxt env e used_vars
  | UOp (_, _) -> synth_uop lin_ctxt env e used_vars
  | Let (x, e1, e2) -> (
      match e1 with
      | Annot (_, ty) -> (
          let e1', t1, e1_vars = synth lin_ctxt ((x, ty) :: env) e1 used_vars in
          let e2', t2, e2_vars = synth lin_ctxt ((x, t1) :: env) e2 e1_vars in
          match List.find_opt (fun a -> a = x) e2_vars with
          | None -> (Let ("_", e1', e2'), t2, e2_vars)
          | Some _ -> (Let (x, e1', e2'), t2, e2_vars))
      | _ -> (
          let e1', t1, e1_vars = synth lin_ctxt env e1 used_vars in
          let e2', t2, e2_vars = synth lin_ctxt ((x, t1) :: env) e2 e1_vars in
          match List.find_opt (fun a -> a = x) e2_vars with
          | None -> (Let ("_", e1', e2'), t2, e2_vars)
          | Some _ -> (Let (x, e1', e2'), t2, e2_vars)))
  | FunApp (e1, e2) -> (
      let e1', ty, e1_vars = synth lin_ctxt env e1 used_vars in
      match ty with
      | TFun (t1, t2) ->
          let e2', _, e2_vars = check lin_ctxt env e2 t1 e1_vars in
          (FunApp (e1', e2'), t2, e2_vars)
          (* Success *)
      | _ -> error (NotFunctionType ty))
  | Annot (e, ty) -> check lin_ctxt env e ty used_vars
  | Cond (cond, e1, e2) ->
      let cond', _, cond_vars = check lin_ctxt env cond TBool used_vars in
      let e1', t1, e1_vars = synth lin_ctxt env e1 cond_vars in
      let e2', _, e2_vars = check lin_ctxt env e2 t1 e1_vars in
      (Cond (cond', e1', e2'), t1, e2_vars)
  | ProcExp (chan, proc, _, _) ->
      let out_ctxt, p', st, vars = synth_proc lin_ctxt env proc chan used_vars in
      if out_ctxt = [] then
        (ProcExp (chan, p', Some st, lin_ctxt), TProc (st, lin_ctxt), vars)
      else error NonEmptyLinearContext
  | ExecExp exp -> (
      let e', ty, vars = synth lin_ctxt env exp used_vars in
      match ty with TProc _ -> (ExecExp e', ty, vars) | _ -> error (NotProcessType ty))
  | RecFunDef (_, _, _, _) ->
      assert false (* RecFunDef does not happen in  the typechecker fase*)

(** This is the process synthesizing function.
   We don't have an explicit check_proc function, such tests are done by means of subtyping. 
   NOTA Consider adding check_proc. (João)

   @param lin_ctxt the linear context under which type synthesis must take place
   @param env the functional environment needed to type functional expressions (spawning processes or sending a functional value)
   @param process the process to type
   @param c the name of teh channel that the process is offering its session on
   @param used_vars a collection of used identifiers for compilation purposes
   @return a tuple with the output channel context, the typed process, the actual session type, and a list of used identifiers
*)
and synth_proc lin_ctxt env proc c used_vars =
  match proc with
  | Recv (v, c', Some t, p) -> (
      if c' = c then
        let out_ctxt, p', st, vars = synth_proc lin_ctxt ((v, t) :: env) p c used_vars in
        match List.find_opt (fun a -> a = v) vars with
        | None -> (out_ctxt, Recv ("_", c', Some t, p'), STRecv (t, st), vars)
        | Some _ -> (out_ctxt, Recv (v, c', Some t, p'), STRecv (t, st), vars)
      else
        match List.assoc_opt c' lin_ctxt with
        | Some folded -> (
            match unfold folded with
            | STSend (t', st) ->
                if ty_eq t t' then
                  let new_lin_ctxt = (c', st) :: List.remove_assoc c' lin_ctxt in
                  let out_ctxt, p', st', vars =
                    synth_proc new_lin_ctxt ((v, t) :: env) p c used_vars
                  in
                  match List.find_opt (fun a -> a = v) vars with
                  | None -> (out_ctxt, Recv ("_", c', Some t, p'), st', vars)
                  | Some _ -> (out_ctxt, Recv (v, c', Some t, p'), st', vars)
                else error (UnexpectedType (t, t'))
            | wrong -> error (ChannelHasWrongSType (c', wrong)))
        | None -> error (NoSuchChannelInContext c'))
  | Recv (_, _, None, _) -> error CannotInferType
  | Send (c', e, _, p) -> (
      if (*matches the process to be typed with a process that sends a value*)
         c' = c then
        (* First there is a check of whether the process is sending a values over its own channel, or another channel's process*)
        (* If it is over its own channel then the continuation {!p} has its type synthesized, returning a tuple with the output context, {!p'}, which is the same as {p}, but it (as well as it continuation and so on) may be tagged with additional typing information, session type {!st}, and used variables *)
        let out_ctxt, p', st, p_vars = synth_proc lin_ctxt env p c used_vars in
        (*The functional value to be sent, th synthesis function is called for { !e}, which returns {! e'}, again possibly enriched with typing information, its type {! t}, and more used variables. *)
        let e', t, e_vars = synth [] env e used_vars in
        (* returns the output context of the synthesis, a version of {! process} supplemented with type {! t} as well as {! e'} and {! p'}, the session type that was synthesized {t ^ st}, and a concatenation of all used variables. *)
        (out_ctxt, Send (c', e', Some t, p'), STSend (t, st), p_vars @ e_vars)
      else
        (* If the process is sending a value over a channel other than its own, the session type matching that other channel is pulled from the linear context (there is an error if no such channel-type association exists in the linear context). *)
        match List.assoc_opt c' lin_ctxt with
        | Some folded -> (
            (*The session type is unfolded once, if it is a recursive type, to access the inner nonrecursive session type*)
            match unfold folded with
            | STRecv (t, st) -> (
                (* The unfolded session type is matched with the type t ⊃ st (otherwise there is a typing error), meaning there is an available channel to receive a functional value of type t. *)
                match check [] env e t used_vars with
                (* Next, the functional value {e} is checked against {t}, returning {e'}, the augmented version of {e}, the type {e}, and a list of variables used by the functional value. *)
                | e', _, e_vars ->
                    (* The linear context is updated: channel {c'} is no longer associated to session type t ⊃ st, and is linked to type st.*)
                    let new_lin_ctxt = (c', st) :: List.remove_assoc c' lin_ctxt in
                    (* The next step is the type synthesis of continuation {p}, under the new linear context. The return value is a tuple containing the output context, type enriched {p'}, session type {st'}, and a list of variables used by {p}. *)
                    let out_ctxt, p', st', p_vars =
                      synth_proc new_lin_ctxt env p c used_vars
                    in
                    (* function returns the output linear context; a version of {process}supplemented with {e'}, type {t}, and {p'}; the type of {process}, and a concatenation of all used variables for compilation purposes *)
                    (out_ctxt, Send (c', e', Some t, p'), st', p_vars @ e_vars))
            | wrong -> error (ChannelHasWrongSType (c', wrong)))
        | None -> error (NoSuchChannelInContext c'))
  | Wait (c', p) -> (
      if
        (*Matches the process to one waiting on a given channel {c'}, before continue as {p}*)
        c' = c
      then error (CannotWaitOwnChannel c')
        (*If a channel tries to wait on its own channel, there is a typing error*)
        (* o c' *Nunca* pode ser o c *)
      else
        (* Otherwise the type matching channel {c'} is taken from the linear context and unfolded due to the possibility of beguin a recursive session type. If there is no matching ot the matching types is not {1} then an error is thrown*)
        match List.assoc_opt c' lin_ctxt with
        | Some folded -> (
            match unfold folded with
            | STEnd ->
                (* Given that the processes is waiting on a channel matching type {1} the linear context is altered to remove the {c'-1} type association, ad there is the synthesis of session type for continuation {p} *)
                let new_lin_ctxt = List.remove_assoc c' lin_ctxt in
                let out_ctxt, p', st, vars = synth_proc new_lin_ctxt env p c used_vars in
                (* The result is the output context, a typed labeled {p'}, session type {st} and a list of variables used in {p}. The function returns the output context, a process that waits on channel {c'} and continues as {p'}, type {st} and the list of used variables for the purpose of compilation *)
                (out_ctxt, Wait (c', p'), st, vars)
            | st ->
                error (CannotWaitChannelOfType (c', st))
                (* the channel to wait on is not of STEnd*))
        | None -> error (NoSuchChannelInContext c')
        (* Trying to close something that is not in lin_ctxt *))
  | Fwd (_, d, c') -> (
      if d <> c then
        (* c' esta no contexto, d tem de ser o c *)
        error (InvalidForwardChannel d)
      else
        match List.assoc_opt c' lin_ctxt with
        | Some st' ->
            (List.remove_assoc c' lin_ctxt, Fwd (Some st', d, c'), st', used_vars)
            (* changing the forward subtype *)
        | None -> error (NoSuchChannelInContext c'))
  | Close c' ->
      (*Matches the process to type to a process that closes its channel and shuts down communication*)
      if c' = c then (lin_ctxt, proc, STEnd, used_vars)
        (* If the process is attempting to close its own channel the function will return the linear context (unused), the typed process itself, the session type {1} and the list of used variables*)
      else error (CannotCloseUnownedChannel c')
        (*If the process is attempting to close a channel other than its own then there is a typing error*)
  | Spawn (c', e, _, p, args) -> (
      if
        (* Matches the process to a process that spawns another process on channel {c'} from functional value {e}, with continuation {p}, and taking as context the channels in {args}.*)
        c' = c
      then (* can't spawn on an existing channel *)
        error (ChannelAlreadyExists c')
      else
        match List.assoc_opt c' lin_ctxt with
        (* If the process attempts to spawn on an existing channel, an error is thrown  *)
        | Some _ -> error (ChannelAlreadyExists c')
        | None -> (
            (* The first step is splitting the linear context into disjoint contexts:
               ->  {spawn_exp_ctxt} to pass to the synthesis function that types the spawned process from the functional value;
               -> new_lin_ctxt that will be modified and passed too the application of the synthesis of {e}, with the specific {spawn_exp_ctxt} which returns the labeled {e'}, type {t} and a list of used variables in {e}
            *)
            let spawn_exp_ctxt, next_lin_ctxt = split_ctxt_for_spawn args lin_ctxt in
            let e', t, e_vars = synth spawn_exp_ctxt env e used_vars in
            match t with
            | TProc (st, _) ->
                (* The association of {c'} with type {st} is added to {new_lin_ctxt} to form a new linear context  *)
                let new_lin_ctxt = (c', st) :: next_lin_ctxt in
                (* There is a synthesis of the type of continuation {p} which returns the output context, type augmented {p'} {st'} and a list of variables used in {p} *)
                let out_ctxt, p', st', p_vars =
                  synth_proc new_lin_ctxt env p c used_vars
                in
                (* The functions returns  the output context, a process with continuation {p'} that spawns from {e'} on channel {e'} using channels in {args}, another process of type {st}, session type {st'} and a list of all used variables*)
                (out_ctxt, Spawn (c', e', Some st, p', args), st', e_vars @ p_vars)
            (* If type {t} does not match a process type ( trying to spawn something that is not a process, an error is thrown) *)
            | ty -> error (NotProcessType ty)))
  | TailSpawn (_, _, _, _, _, _, _) ->
      assert false (*TailSpawn only occurs in preprocessor phase, after typechecking*)
  | Choice (c', l) -> (
      if (* Matches the process to a process offering a choice session *)
         c' = c then
        (* If the choice is on the process' own channel {c} then we are dealing with an external choice, meaning that the process will have its behavior dictated by another *)
        (* external choice; server offers choice of behaviour *)
        let ctxt_opt, typed_pairs, proc_pairs, vars =
          synth_external_choice_proc_list lin_ctxt env c l used_vars
        in
        match ctxt_opt with
        | None -> error EmptyChoice
        | Some ctxt ->
            (* After making sure the choice is not empty, meaning there are label-process pairs in the choice, the function returns the output context, the annotated choice process and its type , and a list of used  variables*)
            (ctxt, Choice (c', proc_pairs), STExtChoice typed_pairs, vars)
      else
        (* internal choice; client must deal with a choice made by the server *)
        (* The choice refers to a process that exists in the linear context, then the function will first unfold the process' type, int case ir is a recursive type *)
        match List.assoc_opt c' lin_ctxt with
        | Some folded -> (
            match unfold folded with
            | STIntChoice choice_pairs -> (
                let ctxt_opt, st_opt, new_procs, vars =
                  synth_internal_choice_proc_list lin_ctxt env c c' l choice_pairs
                    used_vars
                in
                match ctxt_opt with
                | None -> error EmptyChoice
                | Some ctxt -> (
                    match st_opt with
                    | None -> error EmptyChoice
                    | Some st -> (ctxt, Choice (c', new_procs), st, vars)))
            | wrong -> error (ChannelHasWrongSType (c', wrong)))
        | None -> error (NoSuchChannelInContext c'))
  (*
   escolhas  / enviar etiqueta / receber etiqueta:
   c:&{l1 : T1 , l2 : T2 , ... }     canal pronto para receber l1 e prosseguir como T1, ou receber l2 e prosseguir como T2 ....
   c:+{ l1 : T1 , l2 : T2 , ...}     canal onde se envia l1 e segue-se como T1, ou envia-se l2 e segue-se como T2, ...
  *)
  | Label (c', l, p, _) -> (
      if c' = c then
        (* server will choose own behaviour *)
        let out_ctxt, p', st, vars = synth_proc lin_ctxt env p c used_vars in
        (out_ctxt, Label (c', l, p', Some st), STIntChoice [ (l, st) ], vars)
      else
        (* client will choose server behaviour *)
        match List.assoc_opt c' lin_ctxt with
        | Some folded -> (
            match unfold folded with
            | STExtChoice choice_pairs -> (
                match List.assoc_opt l choice_pairs with
                | None -> error (NoSuchLabelInType (l, STExtChoice choice_pairs))
                | Some st ->
                    let new_lin_ctxt = (c', st) :: List.remove_assoc c' lin_ctxt in
                    (* c' now has the behaviour associated with label l *)
                    let out_ctxt, p', st', vars =
                      synth_proc new_lin_ctxt env p c used_vars
                    in
                    (out_ctxt, Label (c', l, p', Some st), st', vars))
            | wrong -> error (ChannelHasWrongSType (c', wrong)))
        | None -> error (NoSuchChannelInContext c'))
  | RecvChan (v, c', Some st, p) -> (
      if c' = c then
        let out_ctxt, p', st', vars =
          synth_proc ((v, st) :: lin_ctxt) env p c used_vars
        in
        (out_ctxt, RecvChan (v, c', Some st, p'), STRecvChan (st, st'), vars)
      else
        match List.assoc_opt c' lin_ctxt with
        | Some folded -> (
            match unfold folded with
            | STSendChan (st1, st2) ->
                if subtyping [] st st1 || subtyping [] st1 st then
                  let new_lin_ctxt =
                    (v, st) :: (c', st2) :: List.remove_assoc c' lin_ctxt
                  in
                  let out_ctxt, p', st', vars =
                    synth_proc new_lin_ctxt env p c used_vars
                  in
                  (out_ctxt, RecvChan (v, c', Some st, p'), st', vars)
                else error (UnexpectedSType (st, st1))
            | wrong -> error (ChannelHasWrongSType (c', wrong)))
        | None -> error (NoSuchChannelInContext c'))
  | RecvChan (_, _, None, _) -> error CannotInferType
  | SendChan (c', v, _, p) -> (
      if c' = c then
        match List.assoc_opt v lin_ctxt with
        | Some st ->
            let out_ctxt, p', st', vars =
              synth_proc (List.remove_assoc v lin_ctxt) env p c used_vars
            in
            (out_ctxt, SendChan (c', v, Some st, p'), STSendChan (st, st'), vars)
        | None -> error (NoSuchChannelInContext v)
      else
        match List.assoc_opt c' lin_ctxt with
        | Some folded -> (
            match unfold folded with
            | STRecvChan (st1, st2) -> (
                match List.assoc_opt v lin_ctxt with
                | Some st ->
                    if subtyping [] st st1 || subtyping [] st1 st then
                      let new_lin_ctxt =
                        (c', st2) :: List.remove_assoc c' (List.remove_assoc v lin_ctxt)
                      in
                      let out_ctxt, p', st', vars =
                        synth_proc new_lin_ctxt env p c used_vars
                      in
                      (out_ctxt, SendChan (c', v, Some st, p'), st', vars)
                    else error (UnexpectedSType (st, st1))
                | None -> error (NoSuchChannelInContext v))
            | wrong -> error (ChannelHasWrongSType (c', wrong)))
        | None -> error (NoSuchChannelInContext c'))
  | Print (e, p) ->
      let e', _, e_vars = synth [] env e used_vars in
      let out_ctxt, p', st', p_vars = synth_proc lin_ctxt env p c used_vars in
      (out_ctxt, Print (e', p'), st', e_vars @ p_vars)
  | If (e, p1, p2) ->
      let e', _, e_vars = check lin_ctxt env e TBool used_vars in
      let p1_ctxt, p1', st1, p1_vars = synth_proc lin_ctxt env p1 c used_vars in
      let p2_ctxt, p2', st2, p2_vars = synth_proc lin_ctxt env p2 c used_vars in
      if subtyping [] st1 st2 || subtyping [] st2 st1 then
        if p1_ctxt = p2_ctxt then
          (p1_ctxt, If (e', p1', p2'), st1, e_vars @ p1_vars @ p2_vars)
        else error AllCasesMustProduceIdenticalCtxt
      else error (UnexpectedSType (st1, st2))
  | MultiSend (_, _, _, _) | MultiRecv (_, _, _, _) ->
      assert false (*not possible in typechecker phase*)

(** Create ctxt to evalute spawn exp, while at the same type removing respective entries from lin_ctxt *)
and split_ctxt_for_spawn args lin_ctxt =
  let fold_fun (spawn_exp_ctxt, next_lin_ctxt) a =
    match List.assoc_opt a next_lin_ctxt with
    | Some st -> ((a, st) :: spawn_exp_ctxt, List.remove_assoc a next_lin_ctxt)
    | None -> error (NoSuchChannelInContext a)
  in
  List.fold_left fold_fun ([], lin_ctxt) args

(** This function type checks an entire external choice's label-process pairs list and returns the resulting checked list.
   The label-process pairs are annotated with the process' type for compilation purposes.
   Note that the type checking of every process must always produce the same linear context.

  Types all the label-process pairs in the choice, 
  @return the output context, the label-type pairs resulting from synthesis, the annotated process pairs and a list of used variables for use in the compilation phase 
*)
and synth_external_choice_proc_list lin_ctxt env c plist used_vars =
  let fold_fun (ctxt_opt, typed_pairs, proc_pairs, curr_vars) (v, (p, _)) =
    let other_ctx, p', st, vars = synth_proc lin_ctxt env p c curr_vars in
    let new_typed_pairs = (v, st) :: typed_pairs in
    let new_proc_pairs = (v, (p', Some st)) :: proc_pairs in
    match ctxt_opt with
    | None -> (Some other_ctx, new_typed_pairs, new_proc_pairs, vars)
    | Some ctxt ->
        if ctxt = other_ctx then (Some other_ctx, new_typed_pairs, new_proc_pairs, vars)
        else error AllCasesMustProduceIdenticalCtxt
  in
  List.fold_left fold_fun (None, [], [], used_vars) plist

(** This function type checks the choice process that is written in response to an internal choice.
   There must be, at least, a matching label for every label of the external choice type.
   All the label-process type checks must produce the same type and the same linear context.
   Types the internal choice' label pairs, while ensuring the choice has sufficient label-process pairs to match the internal choice session
   @return the output context the type in the label-process pairs, the annotated processes and the list of used variables *)
and synth_internal_choice_proc_list lin_ctxt env c c' proc_list sty_list used_vars =
  let clean_lin_ctxt = List.remove_assoc c' lin_ctxt in
  let fold_fun (ctxt_opt, sty_opt, proc_pairs, curr_vars) (v, st) =
    match List.assoc_opt v proc_list with
    | None -> error (NoCaseForLabel v)
    | Some (p, _) -> (
        let out_ctxt, p', st', vars =
          synth_proc ((c', st) :: clean_lin_ctxt) env p c curr_vars
        in
        match ctxt_opt with
        | None -> (
            match sty_opt with
            | None -> (Some out_ctxt, Some st', (v, (p', Some st)) :: proc_pairs, vars)
            | Some stype -> (
                match stype with
                | STIntChoice stype_pairs -> (
                    match st' with
                    (* This part here accounts for the posssibility of an choice process with several cases,
                        where each one returns a different internal choice type. Since choice type +{a:T} is a subtype(?) of +{a:T, b:U},
                        we can return the final type of the choice process as a concatenation of the internal choices of each case (accounting for repetition of labels).
                        So if we have:
                         case c of
                         l1:(c.a)
                         l2:(c.a; c.b)

                       The resulting type of the choice process is +{a:T, b:U}
                    *)
                    | STIntChoice st'_pairs ->
                        let full_pair_list =
                          join_int_choice_type_lists stype_pairs st'_pairs
                        in
                        ( Some out_ctxt,
                          Some (STIntChoice full_pair_list),
                          (v, (p', Some st)) :: proc_pairs,
                          vars )
                    | _ ->
                        if subtyping [] st' stype || subtyping [] stype st' then
                          (Some out_ctxt, Some st', (v, (p', Some st)) :: proc_pairs, vars)
                        else error (AllCasesMustProduceIdenticalType (stype, st')))
                | _ ->
                    if subtyping [] st' stype || subtyping [] stype st' then
                      (Some out_ctxt, Some st', (v, (p', Some st)) :: proc_pairs, vars)
                    else error (AllCasesMustProduceIdenticalType (stype, st'))))
        | Some ctxt ->
            if out_ctxt = ctxt then
              match sty_opt with
              | None -> (Some out_ctxt, Some st', (v, (p', Some st)) :: proc_pairs, vars)
              | Some stype -> (
                  match stype with
                  | STIntChoice stype_pairs -> (
                      match st' with
                      | STIntChoice st'_pairs ->
                          ( Some out_ctxt,
                            Some (STIntChoice (stype_pairs @ st'_pairs)),
                            (v, (p', Some st)) :: proc_pairs,
                            vars )
                      | _ ->
                          if subtyping [] st' stype || subtyping [] stype st' then
                            ( Some out_ctxt,
                              Some st',
                              (v, (p', Some st)) :: proc_pairs,
                              vars )
                          else error (AllCasesMustProduceIdenticalType (stype, st')))
                  | _ ->
                      if subtyping [] st' stype || subtyping [] stype st' then
                        (Some out_ctxt, Some st', (v, (p', Some st)) :: proc_pairs, vars)
                      else error (AllCasesMustProduceIdenticalType (stype, st')))
            else error AllCasesMustProduceIdenticalCtxt)
  in
  List.fold_left fold_fun (None, None, [], used_vars) sty_list

(** This function concats two lists of choice pairs, accounting for possible repetition of labels in both lists *)
and join_int_choice_type_lists la lb =
  let fold_fun ls (label_a, stype) =
    match List.assoc_opt label_a ls with
    | Some st ->
        if subtyping [] stype st || subtyping [] st stype then ls
        else error (AllCasesMustProduceIdenticalType (stype, st))
    | None -> (label_a, stype) :: ls
  in
  List.fold_left fold_fun lb la

and synth_bop lin_ctxt env e used_vars =
  match e with
  | BOp (op, e1, e2) -> (
      match op with
      | Mul | Div | Add | Sub -> (
          let e1', t1, e1_vars = check lin_ctxt env e1 TNum used_vars in
          let e2', t2, e2_vars = check lin_ctxt env e2 TNum e1_vars in
          match (t1, t2) with
          | TNum, TNum -> (BOp (op, e1', e2'), TNum, e2_vars)
          | TNum, _ -> error (UnexpectedType (t2, TNum))
          | _, _ -> error (UnexpectedType (t1, TNum)))
      | And | Or -> (
          let e1', t1, e1_vars = check lin_ctxt env e1 TBool used_vars in
          let e2', t2, e2_vars = check lin_ctxt env e2 TBool e1_vars in
          match (t1, t2) with
          | TBool, TBool -> (BOp (op, e1', e2'), TBool, e2_vars)
          | TBool, _ -> error (UnexpectedType (t2, TBool))
          | _, _ -> error (UnexpectedType (t1, TBool)))
      | Lesser | Greater | Equals -> (
          let e1', t1, e1_vars = synth lin_ctxt env e1 used_vars in
          let e2', t2, e2_vars = synth lin_ctxt env e2 e1_vars in
          match (t1, t2) with
          | TNum, TNum -> (BOp (op, e1', e2'), TBool, e2_vars)
          | TBool, TBool -> (BOp (op, e1', e2'), TBool, e2_vars)
          | TNum, _ -> error (UnexpectedType (t2, TNum))
          | _, TNum -> error (UnexpectedType (t1, TNum))
          | TBool, _ -> error (UnexpectedType (t2, TBool))
          | _, TBool -> error (UnexpectedType (t1, TBool))
          | _, _ -> error (NotBinaryOp e)))
  | _ -> error (NotBinaryOp e)

and synth_uop lin_ctxt env e used_vars =
  match e with
  | UOp (op, exp) -> (
      match op with
      | Neg -> (
          let e', t, vars = check lin_ctxt env exp TNum used_vars in
          match t with
          | TNum -> (UOp (op, e'), TNum, vars)
          | _ -> error (UnexpectedType (t, TNum)))
      | Not -> (
          let e', t, vars = check lin_ctxt env exp TBool used_vars in
          match t with
          | TBool -> (UOp (op, e'), TBool, vars)
          | _ -> error (UnexpectedType (t, TBool))))
  | _ -> error (NotUnaryOp e)

(** Expand a given custom type into its actual primitive type.
   A custom type being a user defined type; the type of a variable representing another type.
*)
let rec expand_custom_type ty env =
  match ty with
  | TUnit | TNum | TBool -> ty
  | TProc (st, l) -> TProc (expand_custom_stype st env, expand_lin_ctxt l env)
  | TFun (x, y) -> TFun (expand_custom_type x env, expand_custom_type y env)
  | TVar v -> (
      match List.assoc_opt v env with
      | Some t' -> expand_custom_type t' env
      | None -> error (NoSuchArg v))

and expand_custom_stype sty env =
  match sty with
  | STEnd | STVar _ -> sty
  | STExtChoice l -> STExtChoice (expand_lin_ctxt l env)
  | STIntChoice l -> STIntChoice (expand_lin_ctxt l env)
  | STSend (t, st) -> STSend (expand_custom_type t env, expand_custom_stype st env)
  | STRecv (t, st) -> STRecv (expand_custom_type t env, expand_custom_stype st env)
  | STSendChan (st1, st2) ->
      STSendChan (expand_custom_stype st1 env, expand_custom_stype st2 env)
  | STRecvChan (st1, st2) ->
      STRecvChan (expand_custom_stype st1 env, expand_custom_stype st2 env)
  | STRec (v, st) -> STRec (v, expand_custom_stype st env)
  | STUVar v -> (
      match List.assoc_opt v env with
      | Some (TProc (st, _)) -> expand_custom_stype st env
      | _ -> error (NoSuchArg v))
  | STMultiSend _ | STMultiRecv _ -> assert false (*possible in typechecker phase*)

and expand_lin_ctxt ctxt env =
  let fold_fun n_ctxt (v, st) = (v, expand_custom_stype st env) :: n_ctxt in
  List.fold_left fold_fun [] ctxt

and expand_custom_exp exp env =
  match exp with
  | UnitVal | Num _ | Bool _ | Var _ -> exp
  | FunDef (x, Some t1, b, Some t2) ->
      FunDef (x, Some (expand_custom_type t1 env), b, Some (expand_custom_type t2 env))
  | FunDef (x, Some t1, b, None) -> FunDef (x, Some (expand_custom_type t1 env), b, None)
  | FunDef (x, None, b, Some t2) ->
      FunDef (x, None, expand_custom_exp b env, Some (expand_custom_type t2 env))
  | FunDef (x, None, b, None) -> FunDef (x, None, expand_custom_exp b env, None)
  | BOp (op, e1, e2) -> BOp (op, expand_custom_exp e1 env, expand_custom_exp e2 env)
  | UOp (op, e) -> UOp (op, expand_custom_exp e env)
  | Let (x, e1, e2) -> Let (x, expand_custom_exp e1 env, expand_custom_exp e2 env)
  | FunApp (e1, e2) -> FunApp (expand_custom_exp e1 env, expand_custom_exp e2 env)
  | Annot (e, ty) -> Annot (expand_custom_exp e env, expand_custom_type ty env)
  | Cond (cond, e1, e2) ->
      Cond (expand_custom_exp cond env, expand_custom_exp e1 env, expand_custom_exp e2 env)
  | ProcExp (chan, proc, Some st, ctxt) ->
      ProcExp
        ( chan,
          expand_custom_proc proc env,
          Some (expand_custom_stype st env),
          expand_lin_ctxt ctxt env )
  | ProcExp (chan, proc, None, ctxt) ->
      ProcExp (chan, expand_custom_proc proc env, None, expand_lin_ctxt ctxt env)
  | ExecExp exp -> ExecExp (expand_custom_exp exp env)
  | RecFunDef (_, _, _, _) ->
      assert false (* RecFunDef does not happen in  the typechecker fase*)

and expand_custom_proc proc env =
  match proc with
  | Send (var, exp, Some ty, p) ->
      Send
        ( var,
          expand_custom_exp exp env,
          Some (expand_custom_type ty env),
          expand_custom_proc p env )
  | Send (var, exp, None, p) ->
      Send (var, expand_custom_exp exp env, None, expand_custom_proc p env)
  | Recv (v1, v2, Some ty, p) ->
      Recv (v1, v2, Some (expand_custom_type ty env), expand_custom_proc p env)
  | Recv (v1, v2, None, p) -> Recv (v1, v2, None, expand_custom_proc p env)
  | Close _ -> proc
  | Wait (var, p) -> Wait (var, expand_custom_proc p env)
  | Fwd (Some st, c, d) -> Fwd (Some (expand_custom_stype st env), c, d)
  | Fwd (None, c, d) -> Fwd (None, c, d)
  | Spawn (var, exp, Some st, p, l) ->
      Spawn
        ( var,
          expand_custom_exp exp env,
          Some (expand_custom_stype st env),
          expand_custom_proc p env,
          l )
  | Spawn (var, exp, None, p, l) ->
      Spawn (var, expand_custom_exp exp env, None, expand_custom_proc p env, l)
  | TailSpawn (_, _, _, _, _, _, _) ->
      assert false (*TailSpawn only occurs in preprocessor phase, after typechecking*)
  | Choice (c, l) -> Choice (c, expand_custom_choice_list l env)
  | Label (v1, v2, p, Some st) ->
      Label (v1, v2, expand_custom_proc p env, Some (expand_custom_stype st env))
  | Label (v1, v2, p, None) -> Label (v1, v2, expand_custom_proc p env, None)
  | SendChan (v1, v2, Some st, p) ->
      SendChan (v1, v2, Some (expand_custom_stype st env), expand_custom_proc p env)
  | SendChan (v1, v2, None, p) -> SendChan (v1, v2, None, expand_custom_proc p env)
  | RecvChan (v1, v2, Some st, p) ->
      RecvChan (v1, v2, Some (expand_custom_stype st env), expand_custom_proc p env)
  | RecvChan (v1, v2, None, p) -> RecvChan (v1, v2, None, expand_custom_proc p env)
  | Print (e, p) -> Print (expand_custom_exp e env, expand_custom_proc p env)
  | If (e, p1, p2) ->
      If (expand_custom_exp e env, expand_custom_proc p1 env, expand_custom_proc p2 env)
  | MultiSend (_, _, _, _) | MultiRecv (_, _, _, _) -> assert false
(* MultiSend and MultiRecv only occur in preprocessor phase, after typechecking*)

and expand_custom_choice_list lst env =
  let fold_fun n_list (s, (p, opt)) =
    match opt with
    | Some st ->
        (s, (expand_custom_proc p env, Some (expand_custom_stype st env))) :: n_list
    | None -> (s, (expand_custom_proc p env, None)) :: n_list
  in
  List.fold_left fold_fun [] lst

(** This function checks and returns the updated declarations and the updated expression.
     As an intermediate step it also computes the env and expands custom types as it checks each declaration .
   The process of checking a declaration implies a permanent expansion of custom types, necessary in a future compilation pass. *)
let check_decl env d =
  match d with
  | Decl (x, ty, exp) ->
      let expanded_type = expand_custom_type ty env in
      let nenv = (x, expanded_type) :: List.remove_assoc x env in
      let expanded_exp = expand_custom_exp exp nenv in
      let nexp, ty', _ = check [] nenv expanded_exp expanded_type [] in
      (x, ty', nexp)

(** @return updated declarations, updated exp, checked exp type

    Takes a program as an argument and returns a tuple of updated declarations, updated expression and checked expression type. The function uses pattern matching to deconstruct the program into a list of declarations and an expression. It then uses {!List.fold_left} to iterate over the declarations and check each one using another function  {!check_decl}. It also updates the environment with each declaration’s name and type. Finally, it uses another function {!expand_custom_exp} to expand the expression using the environment, and another function  {!synth} to synthesize its type *)
let check_program prog =
  match prog with
  | Prog (ldecls, e) ->
      let ndecls, env =
        (* List.fold_left is a function that takes another function, an initial value and a list as arguments. It applies the function to the initial value and the first element of the list, then to the result and the second element of the list, and so on until it reaches the end of the list. It returns the final result1. For example, if we use List.fold_left with (+) as the function, 0 as the initial value and [1; 2; 3] as the list, we get 6 as the result2. This is because (+) is applied to 0 and 1, then to 1 and 2, then to 3 and 3. *)
        (* The accumulator is a par of empty lists. We analyze the declaration with the environment that we already seen until now and the we remake the declaration *)
        List.fold_left
          (fun acc decl ->
            let x, ty, exp = check_decl (snd acc) decl in
            (* new checked (and updated) declaration *)
            let ndecls = Decl (x, ty, exp) :: fst acc in
            let nenv = (x, ty) :: snd acc in
            (ndecls, nenv))
          ([], []) ldecls
      in
      (* Goes through the tree, rebuilds what was already there*)
      let expanded_exp = expand_custom_exp e env in
      let e', t', _ = synth [] env expanded_exp [] in
      (List.rev ndecls, e', t')
