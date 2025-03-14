open Sessint
open Lexing
open Printf
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil

(* The function call [attempt1 filename] returns normally only if a syntax
   error has occurred while parsing [filename]. In that case, it returns the
   content of the file. *)
let attempt1 filename : string =
  let text_ref = ref "" in
  try
    (* Read the file; allocate and initialize a lexing buffer. *)
    let ic = open_in ("../../../test/"^filename) in (* Open channel *)
    let lexbuf = Lexing.from_channel ic in
    text_ref := In_channel.input_all ic;
    (* Run the parser. *)
    match Parser.main Lexer.token lexbuf with

    | InputSyntax.Modl(_,_,_) ->
      (* Success. The parser has produced a semantic value [v]. *)
      printf "%s\n%!" !text_ref;
      !text_ref
  
    | exception Lexer.Error msg ->
        (* A lexical error has occurred. *)
        eprintf "%s%!" msg;
        exit 1
  
    | exception Parser.Error ->
        (* A syntax error has occurred. *)
        !text_ref
  with e -> 
    print_endline (Printexc.to_string e);
    raise e



module I = UnitActionsParser.MenhirInterpreter

(* [succeed v] is invoked when the parser has succeeded and produced a
   semantic value [v]. In our setting, this cannot happen, since the
   table-based parser is invoked only when we know that there is a
   syntax error in the input file. *)
let succeed _v =
  assert true

let env checkpoint =
  match checkpoint with
  | I.HandlingError env ->
      env
  | _ ->
      assert false
  
(* [state checkpoint] extracts the number of the current state out of a
   checkpoint. *)
let state checkpoint : int =
  match I.top (env checkpoint) with
  | Some (I.Element (s, _, _, _)) ->
      I.number s
  | None ->
      (* Hmm... The parser is in its initial state. The incremental API
         currently lacks a way of finding out the number of the initial
         state. It is usually 0, so we return 0. This is unsatisfactory
         and should be fixed in the future. *)
      0
  
(* [show text (pos1, pos2)] displays a range of the input text [text]
   delimited by the positions [pos1] and [pos2]. *)
let show text positions =
  E.extract text positions
  |> E.sanitize
  |> E.compress
  |> E.shorten 20 (* max width 43 *)
    
(* [get text checkpoint i] extracts and shows the range of the input text that
   corresponds to the [i]-th stack cell. The top stack cell is numbered zero. *)

let get text checkpoint i =
  match I.get i (env checkpoint) with
  | Some (I.Element (_, _, pos1, pos2)) ->
      show text (pos1, pos2)
  | None ->
      (* The index is out of range. This should not happen if [$i]
         keywords are correctly inside the syntax error message
         database. The integer [i] should always be a valid offset
         into the known suffix of the stack. *)
      "???"
  

(* [fail text buffer checkpoint] is invoked when parser has encountered a
   syntax error. *)
let fail text buffer (checkpoint : _ I.checkpoint) =
  (* Indicate where in the input file the error occurred. *)
  let location = L.range (E.last buffer) in
  (* Show the tokens just before and just after the error. *)
  let indication = sprintf "Syntax error %s.\n" (E.show (show text) buffer) in
  (* Fetch an error message from the database. *)
  let message = ErrorMessages.message (state checkpoint) in
  (* Expand away the $i keywords that might appear in the message. *)
  let message = E.expand (get text checkpoint) message in
  (* Show these three components. *)
  eprintf "%s%s%s%!" location indication message;
  exit 1
  
let rec loop text buffer supplier checkpoint =
  match checkpoint with
  | I.InputNeeded _env ->
      (* The parser needs a token. *)
      let token = supplier () in
      let checkp = I.offer checkpoint token in
      loop text buffer supplier checkp
  
  | I.Shifting _ | I.AboutToReduce _ ->
    (* The parser is shifting or reducing. *)
      let checkp = I.resume checkpoint in
      loop text buffer supplier checkp
  
  | I.HandlingError _env ->
      (* The parser has encountered a syntax error. *)
      fail text buffer checkpoint
  
  | I.Accepted v ->
      (* The parser has succeeded and produced a semantic value [v]. *)
      succeed v
  
  | I.Rejected ->
      (* The parser has failed. This should not happen. *)
      assert false
  
(* [attempt2 filename text] runs the parser. *)
let attempt2 filename text =
  (* Allocate and initialize a lexing buffer. *)
  let lexbuf = L.init (filename) (Lexing.from_string text) in
  (* Wrap the lexer and lexbuf together into a supplier, that is, a
     function of type [unit -> token * position * position]. *)
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  (* Equip the supplier with a two-place buffer that records the positions
     of the last two tokens. This is useful when a syntax error occurs, as
     these are the token just before and just after the error. *)
  let buffer, supplier = E.wrap_supplier supplier in
  (* Fetch the parser's initial checkpoint. *)
  let checkpoint = UnitActionsParser.Incremental.main lexbuf.lex_curr_p in
  (* Run the parser. *)
  (* We do not handle [Lexer.Error] because we know that we will not
     encounter a lexical error during this second parsing run. *)
  loop text buffer supplier checkpoint

let rec retrieve_names_not_in_list filename_lst =
  try
  let found_files = List.fold_left (fun acc filename -> let ic = open_in ("../../../test/"^filename) in
                                      let lexbuf = Lexing.from_channel ic in
                                      let modl = Parser.main Lexer.token lexbuf in
                                      match modl with
                                      (* Found module *)
                                      | InputSyntax.Modl (_, imprt_lst, _) -> List.fold_left (fun acc imprt ->
                                        match imprt with
                                        | InputSyntax.Imprt (i, _) -> if not (List.exists (fun filename -> filename = i^".prog") filename_lst) then
                                                                        (i^".prog") :: acc
                                                                      else
                                                                        acc
                                      ) acc imprt_lst
  ) [] filename_lst in
  if List.length found_files = 0 then
    filename_lst
  else
    let new_files = List.append filename_lst found_files in
    retrieve_names_not_in_list new_files
  with e ->
    print_endline(Printexc.to_string e);
    raise e

let create_modl_lst filename_lst = 
  List.rev (List.fold_left (fun acc filename -> let ic = open_in ("../../../test/"^filename) in
                                      let lexbuf = Lexing.from_channel ic in
                                      let modl = Parser.main Lexer.token lexbuf in
                                      modl :: acc
  ) [] filename_lst)

let () =
  if Array.length Sys.argv > 1 then
    (* Get files from command line *)
    let filenames = Array.to_list (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)) in
    (* Create full file list *)
    let final_filename_lst = retrieve_names_not_in_list filenames in
    (* Perform parser on all the files*)
    List.iter (fun filename -> let text = attempt1 filename in
                                       if text <> "" then 
                                       attempt2 filename text
    ) final_filename_lst;
    let final_modls = create_modl_lst final_filename_lst in
    print_endline ("Desugaring modules...");
    let syn_modls = List.rev (List.fold_left(fun acc modl ->
      (Syntax.desugar_modl modl) :: acc) [] final_modls) in
    print_endline("Modules have been desugared...");

    print_endline ("Typechecking modules...");
    (* Iterate through the final filenames to perform type checking and compilation *)
    let ch_modls = Typechecker.check_modl_lst syn_modls in
    print_endline ("Modules have been typechecked...");
    
    print_endline ("Compiling modules...");
    let _ = Compiler.compile_modls ch_modls in
    print_endline ("Modules have been compiled...");
else
  print_endline "No files provided"
