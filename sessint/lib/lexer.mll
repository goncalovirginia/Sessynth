{
open Lexing
open Parser

exception UnknownToken of char

let sprintf  = Printf.sprintf

let position lexbuf =
    let p = lexbuf.lex_curr_p in
        sprintf "%s:%d:%d" 
        p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)

exception Error of string
let error lexbuf fmt = 
    Printf.kprintf (fun msg -> 
        raise (Error ((position lexbuf)^" "^msg))) fmt
}

let digit = ['0'-'9']
let bool = ("true" | "false")
let u_char = ['A'-'Z']
let l_char = ['a'-'z']
let char = (l_char|u_char)

rule token = parse
	| [' ' '\t' '\r' '\n' ]  				{ token lexbuf }
	| '+'  				     				{ PLUS } 
	| '*'  					 				{ MULT } 
	| "/"							 		{ DIV }
	| '-'  							 		{ MINUS }
	| "rec"									{ REC }
	| "let"					 				{ LET }
	| "in"					 				{ IN }
	| "and" 				 				{ AND } 
	| "or" 					 				{ OR } 
	| "not" 				 				{ NOT } 
	| "fun"					 				{ FUN }
	| "end"					 				{ END }
	| "if"					 				{ IF }
	| "then"				 				{ THEN }
	| "else"				 				{ ELSE }
	| "endif" 				 				{ ENDIF }
	| "sendc"				 				{ SEND_CHAN }
	| "recvc"				 				{ RECV_CHAN }
	| "send"				 				{ SEND }
	| "recv"				 				{ RECV }
	| "close"				 				{ CLOSE }
	| "wait"				 				{ WAIT }
	| "fwd"					 				{ FWD }
	| "spawn" 						 		{ SPAWN }
	| "print" 								{ PRINT }
	| "case"   				 				{ CASE }
	| "of"								 	{ OF }
	| "type"								{ TYPE }
	| "stype"								{ STYPE }
	| "."					 				{ DOT }
	| '?'					 				{ QUESTION }
	| '@' 									{ END_STYPE }
	| "-o" 									{ LOLLIPOP }
	| "=>" 						 			{ RIGHT_ARROW_BOLD }
	| "^"									{ CIRCUMFLEX }
	| '&'                           		{ AMPERSAND }
	| "->"					 				{ RIGHT_ARROW }
	| "<-"					 				{ LEFT_ARROW }
	| "=" 					 				{ EQUALS } 
	| '<'					 				{ LESSER }
	| '>'					 				{ GREATER }
	| ':'					 				{ COLON }
	| ';'					 				{ SEMI_COLON }
	| ','									{ COMMA }
	| ";;"					 				{ RETURN }
	| "()"									{ UNIT_VAL }
	| '('					 				{ L_PAR }
	| ')'					 				{ R_PAR }
	| '{'					 				{ L_BRACE }
	| '}'					 				{ R_BRACE }
	| ("int" | "num" | "nat") 				{ TNUM }
	| "bool" 			 	 				{ TBOOL }
	| "unit"								{ TUNIT }
	| bool as bool_str   	 				{ BOOL (bool_of_string bool_str) } 
	| digit+ as num          				{ INT (int_of_string num) }
	| l_char (char|digit|'_')* as wd		{ VAR wd }
	| u_char (char|digit|'_')* as st		{ S_VAR st }
	| '_'char(char|digit|'_')* as ty		{ T_VAR ty }
	| _ as tk 								{ raise (UnknownToken tk) }
	| eof 					 				{ raise End_of_file }
