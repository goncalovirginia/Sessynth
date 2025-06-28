open Core_bench
open Sessint

let single_channel = true
let logs_on = false
let compile_times = ref false

(** Example: get_file_name "/home/user/documents/report.pdf" will return "report"
    @param path path of the desired file
    @return the base name of a file given a path, removes the directories an the extension*)
let getFileName path =
  let name = Filename.remove_extension path in
  (* remove the extension *)
  Filename.basename name (* return the base file name *)

let compile program_name prog multisend_struct =
  let filename = getFileName program_name in
  (try
     let desugared_prog = Syntax.desugar_prog prog in
     let decls, e, _ty = Typechecker.check_program desugared_prog in
     let prog = Syntax.Prog (decls, e) in
     Compiler.compile_prog prog single_channel multisend_struct !compile_times filename
   with   End_of_file -> ()
   | Printer.TError ex -> print_endline @@ Printer.string_from_err ex)

let compile_with_optimizations program_name prog multisend_struct =
  let filename = getFileName program_name in
  (try

     let desugared_prog = Syntax.desugar_prog prog in
     let decls, e, _ty = Typechecker.check_program desugared_prog in
     let prog = Preprocessor.optimize_representation (Prog (decls, e)) in
     Compiler.compile_prog prog single_channel multisend_struct !compile_times filename
   with   End_of_file -> ()
   | Printer.TError ex -> print_endline @@ Printer.string_from_err ex)


let compute_compile_times prog_name prog use_struct =
  Command_unix.run
    (Bench.make_command
       [
         Bench.Test.create ~name:"Compiler sem otimizações" (fun () ->
             ignore (compile prog_name prog false));
         Bench.Test.create ~name:"Compiler com otimizações" (fun () ->
             ignore (compile_with_optimizations prog_name prog use_struct));
       ])

let () =
    let prog_name = Sys.argv.(1) in
  let prog_file = open_in prog_name in
  let prog = In_channel.input_all prog_file in
  close_in prog_file;
  let lexbuf = (Lexing.from_string prog) in
  let prog = Parser.main Lexer.token lexbuf in
  let use_struct = bool_of_string Sys.argv.(2) in
  compile_times := bool_of_string Sys.argv.(3);
  if !compile_times then (
    Sys.argv.(1) <- "time";
    Sys.argv.(2) <- "samples";
    Sys.argv.(3) <- "speedup";
    compute_compile_times prog_name prog use_struct)
  else (
    if logs_on then (
      Logs.set_reporter (Logs.format_reporter ());
      Logs.set_level (Some Logs.Debug));
    compile_with_optimizations prog_name prog use_struct)
