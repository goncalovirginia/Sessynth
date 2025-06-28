
(* The type of tokens. *)

type token = 
  | WAIT
  | VAR of (string)
  | UNIT_VAL
  | T_VAR of (string)
  | TYPE
  | TUNIT
  | TNUM
  | THEN
  | TBOOL
  | S_VAR of (string)
  | STYPE
  | SPAWN
  | SEND_CHAN
  | SEND
  | SEMI_COLON
  | R_PAR
  | R_BRACE
  | RIGHT_ARROW_BOLD
  | RIGHT_ARROW
  | RETURN
  | RECV_CHAN
  | RECV
  | REC
  | QUESTION
  | PRINT
  | PLUS
  | OR
  | OF
  | NOT
  | MULT
  | MINUS
  | L_PAR
  | L_BRACE
  | LOLLIPOP
  | LET
  | LESSER
  | LEFT_ARROW
  | INT of (int)
  | IN
  | IF
  | GREATER
  | FWD
  | FUN
  | EQUALS
  | END_STYPE
  | ENDIF
  | END
  | ELSE
  | DOT
  | DIV
  | COMMA
  | COLON
  | CLOSE
  | CIRCUMFLEX
  | CASE
  | BOOL of (bool)
  | AND
  | AMPERSAND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (InputSyntax.prog)
