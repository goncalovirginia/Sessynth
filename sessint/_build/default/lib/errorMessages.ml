
(* This file was auto-generated based on "errorMessages.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 253 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid import."
    | 251 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid declaration.\n"
    | 249 ->
        "Syntax error: Expected 'RETURN' after 'exec_exp'.\n"
    | 246 ->
        "Syntax error: Expected an executable expression after 'RETURN'.\n"
    | 243 ->
        "Syntax error: Unexpected token 'WHERE'. Expected end of file.\n"
    | 241 ->
        "Syntax error: Unexpected token 'WHERE'. Expected ';' after 'STYPE S_VAR stype'.\n"
    | 240 ->
        "Syntax error: Expected a structural type after 'STYPE S_VAR'.\n"
    | 239 ->
        "Syntax error: Expected a structural type variable after 'STYPE'.\n"
    | 237 ->
        "Syntax error: Unexpected token 'VAR'. Expected ';' after 'TYPE T_VAR ty'.\n"
    | 236 ->
        "Syntax error: Expected a type after 'TYPE T_VAR'.\n"
    | 235 ->
        "Syntax error: Expected a type variable after 'TYPE'.\n"
    | 233 ->
        "Syntax error: Expected ';' after 'VAR : ty expression'.\n"
    | 231 ->
        "Syntax error: Expected '}' after 'VAR <- {proc'.\n"
    | 228 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid process.\n"
    | 227 ->
        "Syntax error: Expected ';' after 'VAR : stype <- RECV_CHAN VAR'.\n"
    | 226 ->
        "Syntax error: Expected a variable after 'VAR : stype <- RECV_CHAN'.\n"
    | 225 ->
        "Syntax error: Expected 'RECV_CHAN' after 'VAR : stype <-'.\n"
    | 224 ->
        "Syntax error: Expected '<-' after 'VAR : stype'.\n"
    | 222 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid process.\n"
    | 221 ->
        "Syntax error: Expected ';' after 'VAR : TBOOL <- RECV VAR'.\n"
    | 220 ->
        "Syntax error: Expected a variable after 'VAR : TBOOL <- RECV'.\n"
    | 219 ->
        "Syntax error: Expected 'RECV' after 'VAR : TBOOL <-'.\n"
    | 218 ->
        "Syntax error: Unexpected token 'VAR'. Expected '<-' or '^' or '=>'.\n"
    | 217 ->
        "Syntax error: Expected a type after 'VAR :'.\n"
    | 215 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid process.\n"
    | 214 ->
        "Syntax error: Expected ';' after 'VAR . VAR'.\n"
    | 213 ->
        "Syntax error: Expected a variable after 'VAR .'.\n"
    | 211 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid process.\n"
    | 210 ->
        "Syntax error: Expected ';' after 'VAR <- RECV VAR'.\n"
    | 209 ->
        "Syntax error: Expected a variable after 'VAR <- RECV'.\n"
    | 207 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid process.\n"
    | 206 ->
        "Syntax error: Expected ';' after 'VAR <- RECV_CHAN VAR'.\n"
    | 205 ->
        "Syntax error: Expected a variable after 'VAR <- RECV_CHAN'.\n"
    | 203 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid process.\n"
    | 201 ->
        "Syntax error: Unexpected token 'WHERE'. Expected ';' or another process.\n"
    | 200 ->
        "Syntax error: Expected '}' after 'VAR <- SPAWN {proc'.\n"
    | 199 ->
        "Syntax error: Expected a process after 'VAR <- SPAWN {'.\n"
    | 193 ->
        "Syntax error: Expected a process after 'IF expression THEN proc ELSE'.\n"
    | 192 ->
        "Syntax error: Expected 'ELSE' after 'IF expression THEN proc'.\n"
    | 189 ->
        "Syntax error: Unexpected token 'WHERE'. Expected '}' or another case.\n"
    | 188 ->
        "Syntax error: Expected ')' after 'VAR : (proc'.\n"
    | 187 ->
        "Syntax error: Expected a process after 'VAR : ('.\n"
    | 186 ->
        "Syntax error: Expected '(' after 'VAR :'.\n"
    | 185 ->
        "Syntax error: Expected ':' after 'VAR'.\n"
    | 184 ->
        "Syntax error: Expected a case list after 'CASE VAR OF'.\n"
    | 183 ->
        "Syntax error: Expected 'OF' after 'CASE VAR'.\n"
    | 182 ->
        "Syntax error: Expected a variable after 'CASE'.\n"
    | 180 ->
        "Syntax error: Expected a variable after 'CLOSE'.\n"
    | 178 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid process.\n"
    | 177 ->
        "Syntax error: Expected a variable after 'FWD'.\n"
    | 176 ->
        "Syntax error: Expected a process after 'IF expression THEN'.\n"
    | 175 ->
        "Syntax error: Expected 'THEN' after 'IF expression'.\n"
    | 174 ->
        "Syntax error: Expected an expression after 'IF'.\n"
    | 173 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid process.\n"
    | 172 ->
        "Syntax error: Expected ';' after 'PRINT expression'.\n"
    | 171 ->
        "Syntax error: Expected an expression after 'PRINT'.\n"
    | 170 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid process.\n"
    | 169 ->
        "Syntax error: Expected ';' after 'SEND VAR expression'.\n"
    | 168 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid expression.\n"
    | 167 ->
        "Syntax error: Expected a variable after 'SEND'.\n"
    | 166 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid process.\n"
    | 165 ->
        "Syntax error: Unexpected token 'WHERE'. Expected ';' after 'SEND_CHAN VAR VAR'.\n"
    | 164 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid process.\n"
    | 163 ->
        "Syntax error: Expected a variable after 'SEND_CHAN'.\n"
    | 162 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid process.\n"
    | 159 ->
        "Syntax error: Unexpected token 'WHERE'. Expected ';' or a list of variables.\n"
    | 158 ->
        "Syntax error: Expected a list of variables after 'VAR <- SPAWN (expression)'.\n"
    | 157 ->
        "Syntax error: Expected ')' after 'VAR <- SPAWN (expression'.\n"
    | 156 ->
        "Syntax error: Expected ')' after 'L_PAR ty'.\n"
    | 154 ->
        "Syntax error: Expected ')' after 'L_PAR ty expression'.\n"
    | 153 ->
        "Syntax error: Expected ')' after 'L_PAR ty'.\n"
    | 151 ->
        "Syntax error: Expected 'END' after 'LET VAR = expression IN expression'.\n"
    | 150 ->
        "Syntax error: Expected an expression after 'LET VAR = expression IN'.\n"
    | 149 ->
        "Syntax error: Expected 'IN' after 'LET VAR = expression'.\n"
    | 147 ->
        "Syntax error: Expected 'ENDIF' after 'IF expression THEN expression ELSE expression'.\n"
    | 146 ->
        "Syntax error: Expected an expression after 'IF expression THEN expression ELSE'.\n"
    | 145 ->
        "Syntax error: Expected 'ELSE' after 'IF expression THEN expression'.\n"
    | 144 ->
        "Syntax error: Expected an expression after 'IF expression THEN'.\n"
    | 143 ->
        "Syntax error: Expected 'THEN' after 'IF expression'.\n"
    | 141 ->
        "Syntax error: Unexpected token 'WHERE'. Expected '=>'.\n"
    | 139 ->
        "Syntax error: Expected 'END' after 'FUN ... => expression'.\n"
    | 137 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid expression.\n"
    | 136 ->
        "Syntax error: Expected an expression after '='.\n"
    | 135 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid expression.\n"
    | 134 ->
        "Syntax error: Expected an expression after '-'.\n"
    | 133 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid expression.\n"
    | 132 ->
        "Syntax error: Expected an expression after 'AND'.\n"
    | 131 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid expression.\n"
    | 130 ->
        "Syntax error: Expected an expression after '>'.\n"
    | 128 ->
        "Syntax error: Expected an expression after '/'.\n"
    | 127 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid expression.\n"
    | 126 ->
        "Syntax error: Expected an expression after '<'.\n"
    | 121 ->
        "Syntax error: Expected an expression after '*'.\n"
    | 120 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid expression.\n"
    | 119 ->
        "Syntax error: Expected an expression after 'OR'.\n"
    | 118 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid expression.\n"
    | 117 ->
        "Syntax error: Expected an expression after '+'.\n"
    | 115 ->
        "Syntax error: Expected ')' after 'L_PAR expression'.\n"
    | 111 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid expression.\n"
    | 109 ->
        "Syntax error: Expected a simple expression after ':'.\n"
    | 108 ->
        "Syntax error: Expected ':' after 'VAR'.\n"
    | 107 ->
        "Syntax error: Expected a simple expression after '?'.\n"
    | 106 ->
        "Syntax error: Unexpected token 'RETURN'. Expected a valid expression.\n"
    | 105 ->
        "Syntax error: Expected an expression after '('.\n"
    | 103 ->
        "Syntax error: Expected a simple expression after '-'.\n"
    | 101 ->
        "Syntax error: Expected an expression after 'FUN ... =>'.\n"
    | 98 ->
        "Syntax error: Expected ')' after 'VAR : TBOOL'.\n"
    | 97 ->
        "Syntax error: Expected a simple type after 'VAR :'.\n"
    | 96 ->
        "Syntax error: Expected ':' after 'FUN (VAR)'.\n"
    | 95 ->
        "Syntax error: Expected a non-empty list of variables after 'FUN ('.\n"
    | 93 ->
        "Syntax error: Expected a simple type after 'VAR :'.\n"
    | 92 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid function binder.\n"
    | 91 ->
        "Syntax error: Expected a non-empty list of function binders after 'FUN'.\n"
    | 90 ->
        "Syntax error: Expected an expression after 'IF'.\n"
    | 88 ->
        "Syntax error: Expected an expression after 'LET VAR ='.\n"
    | 87 ->
        "Syntax error: Expected '=' after 'LET VAR'.\n"
    | 86 ->
        "Syntax error: Expected a variable after 'LET'.\n"
    | 85 ->
        "Syntax error: Expected a type or expression after '('.\n"
    | 84 ->
        "Syntax error: Expected a type or expression after '('.\n"
    | 80 ->
        "Syntax error: Expected an expression after 'VAR <- SPAWN ('.\n"
    | 79 ->
        "Syntax error: Expected '(' or '{' after 'VAR <- SPAWN'.\n"
    | 78 ->
        "Syntax error: Expected 'RECV', 'SPAWN', or 'RECV_CHAN' after 'VAR <-'.\n"
    | 77 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid process.\n"
    | 76 ->
        "Syntax error: Expected a process after 'WAIT VAR ;'.\n"
    | 75 ->
        "Syntax error: Expected ';' after 'WAIT VAR'.\n"
    | 74 ->
        "Syntax error: Expected a variable after 'WAIT'.\n"
    | 73 ->
        "Syntax error: Expected a process after 'VAR <- {'.\n"
    | 72 ->
        "Syntax error: Expected '{' after 'VAR <-'.\n"
    | 71 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid expression.\n"
    | 70 ->
        "Syntax error: Expected an expression after 'VAR : TBOOL ;'.\n"
    | 69 ->
        "Syntax error: Unexpected token 'VAR'. Expected ')' or '^' or '=>'.\n"
    | 65 ->
        "Syntax error: Expected a linear context after 'VAR COLON stype ,'.\n"
    | 64 ->
        "Syntax error: Unexpected token 'WHERE'. Expected '}' or ','.\n"
    | 63 ->
        "Syntax error: Expected a structural type after 'VAR :'.\n"
    | 62 ->
        "Syntax error: Expected ':' after 'VAR'.\n"
    | 61 ->
        "Syntax error: Expected a linear context after 'L_BRACE stype \226\134\144'.\n"
    | 59 ->
        "Syntax error: Expected '}' or '\226\134\144' after 'L_BRACE stype'.\n"
    | 54 ->
        "Syntax error: Expected a choice list after 'VAR COLON stype ,'.\n"
    | 53 ->
        "Syntax error: Unexpected token 'WHERE'. Expected '}' or ','.\n"
    | 51 ->
        "Syntax error: Expected a structural type after 'L_PAR stype R_PAR \226\138\184'.\n"
    | 49 ->
        "Syntax error: Expected a structural type after 'L_PAR stype R_PAR *'.\n"
    | 48 ->
        "Syntax error: Expected '*' or '\226\138\184' after 'L_PAR stype R_PAR'.\n"
    | 47 ->
        "Syntax error: Expected ')' after 'L_PAR stype'.\n"
    | 45 ->
        "Syntax error: Unexpected token 'WHERE'. Expected '=>'.\n"
    | 44 ->
        "Syntax error: Expected a valid type after '=>'.\n"
    | 42 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid type.\n"
    | 40 ->
        "Syntax error: Expected a structural type after 'TBOOL ^'.\n"
    | 39 ->
        "Syntax error: Unexpected token 'VAR'. Expected '^' or '=>'.\n"
    | 38 ->
        "Syntax error: Expected a structural type after '=>'.\n"
    | 36 ->
        "Syntax error: Unexpected token 'VAR'. Expected ')' or '^' or '=>'.\n"
    | 33 ->
        "Syntax error: Expected a choice list after 'AMPERSAND {'.\n"
    | 32 ->
        "Syntax error: Expected '{' after 'AMPERSAND'.\n"
    | 30 ->
        "Syntax error: Expected a type or structural type after '('.\n"
    | 29 ->
        "Syntax error: Expected a structural type after 'VAR :'.\n"
    | 28 ->
        "Syntax error: Expected ':' after 'VAR'.\n"
    | 27 ->
        "Syntax error: Expected a choice list after 'PLUS {'.\n"
    | 26 ->
        "Syntax error: Expected '{' after 'PLUS'.\n"
    | 25 ->
        "Syntax error: Expected a structural type after 'REC VAR .'.\n"
    | 24 ->
        "Syntax error: Expected '.' after 'REC VAR'.\n"
    | 23 ->
        "Syntax error: Expected a variable name after 'REC'.\n"
    | 20 ->
        "Syntax error: Expected a structural type after '{'.\n"
    | 19 ->
        "Syntax error: Expected a type after '('.\n"
    | 14 ->
        "Syntax error: Expected a type after 'VAR :'.\n"
    | 13 ->
        "Syntax error: Expected ':' after 'VAR'.\n"
    | 10 ->
        "Syntax error: Expected 'END' after 'IMPORT VAR ( ... )'.\n"
    | 9 ->
        "Syntax error: Expected ')' after 'IMPORT VAR ( ...'.\n"
    | 7 ->
        "Syntax error: Unexpected token 'WHERE'. Expected ')' or ':'.\n"
    | 6 ->
        "Syntax error: Expected a non-empty list of variables after 'IMPORT VAR ('.\n"
    | 5 ->
        "Syntax error: Expected '(' after 'IMPORT VAR'.\n"
    | 4 ->
        "Syntax error: Expected a variable name after 'IMPORT'.\n"
    | 3 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a list of imports or program declarations.\n"
    | 2 ->
        "Syntax error: Unexpected token 'WAIT'. Expected 'WHERE'.\n"
    | 1 ->
        "Syntax error: Expected a variable name after 'MODULE'.\n"
    | 0 ->
        "Syntax error: Unexpected token 'WHERE'. Expected a valid module declaration.\n"
    | _ ->
        raise Not_found
