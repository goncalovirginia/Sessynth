open Sessint


let rec prompt lexbuf =
  print_string "> " ;
  flush stdout;
  try
    let prog = Parser.main Lexer.token lexbuf in
      let desugared_prog = Syntax.desugar_prog prog in  
        let (decls, e, ty) = Typechecker.check_program desugared_prog in
          print_endline (Printer.string_from_type ty); 
          (* print_endline (string_from_exp (eval_program (Prog (decls, e)))); TODO Stack Overflow on RetType example *)
          Compiler.compile_prog (Prog (decls, e)) true "./_test/filename.go";
        flush stdout; 
        prompt lexbuf;
  with
  | End_of_file -> exit 0
  | Printer.TError ex -> print_endline @@ Printer.string_from_err ex;;



prompt (Lexing.from_channel stdin)

