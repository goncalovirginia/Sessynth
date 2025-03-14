
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | WHERE
    | WAIT
    | VAR of (
# 5 "lib/parser.mly"
       (string)
# 17 "lib/parser.ml"
  )
    | UNIT_VAL
    | T_VAR of (
# 6 "lib/parser.mly"
       (string)
# 23 "lib/parser.ml"
  )
    | TYPE
    | TUNIT
    | TNUM
    | THEN
    | TBOOL
    | S_VAR of (
# 7 "lib/parser.mly"
       (string)
# 33 "lib/parser.ml"
  )
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
    | MODULE
    | MINUS
    | L_PAR
    | L_BRACE
    | LOLLIPOP
    | LET
    | LESSER
    | LEFT_ARROW
    | INT of (
# 8 "lib/parser.mly"
       (int)
# 66 "lib/parser.ml"
  )
    | IN
    | IMPORT
    | IF
    | GREATER
    | FWD
    | FUN
    | EQUALS
    | EOF
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
    | BOOL of (
# 9 "lib/parser.mly"
       (bool)
# 90 "lib/parser.ml"
  )
    | AND
    | AMPERSAND
  
end

include MenhirBasics

# 1 "lib/parser.mly"
  
  open InputSyntax

# 103 "lib/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState003 : ('s _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 003.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState006 : (('s, _menhir_box_main) _menhir_cell1_IMPORT _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 006.
        Stack shape : IMPORT VAR.
        Start symbol: main. *)

  | MenhirState007 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 007.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState012 : (('s _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_list_imprt_, _menhir_box_main) _menhir_state
    (** State 012.
        Stack shape : VAR list(imprt).
        Start symbol: main. *)

  | MenhirState014 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 014.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState019 : (('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_state
    (** State 019.
        Stack shape : L_PAR.
        Start symbol: main. *)

  | MenhirState020 : (('s, _menhir_box_main) _menhir_cell1_L_BRACE, _menhir_box_main) _menhir_state
    (** State 020.
        Stack shape : L_BRACE.
        Start symbol: main. *)

  | MenhirState025 : (('s, _menhir_box_main) _menhir_cell1_REC _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 025.
        Stack shape : REC VAR.
        Start symbol: main. *)

  | MenhirState027 : (('s, _menhir_box_main) _menhir_cell1_PLUS, _menhir_box_main) _menhir_state
    (** State 027.
        Stack shape : PLUS.
        Start symbol: main. *)

  | MenhirState029 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 029.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState030 : (('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_state
    (** State 030.
        Stack shape : L_PAR.
        Start symbol: main. *)

  | MenhirState033 : (('s, _menhir_box_main) _menhir_cell1_AMPERSAND, _menhir_box_main) _menhir_state
    (** State 033.
        Stack shape : AMPERSAND.
        Start symbol: main. *)

  | MenhirState036 : ((('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_state
    (** State 036.
        Stack shape : L_PAR ty.
        Start symbol: main. *)

  | MenhirState038 : ((('s, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_cell1_RIGHT_ARROW_BOLD, _menhir_box_main) _menhir_state
    (** State 038.
        Stack shape : ty RIGHT_ARROW_BOLD.
        Start symbol: main. *)

  | MenhirState039 : (('s, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_state
    (** State 039.
        Stack shape : ty.
        Start symbol: main. *)

  | MenhirState040 : ((('s, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_cell1_CIRCUMFLEX, _menhir_box_main) _menhir_state
    (** State 040.
        Stack shape : ty CIRCUMFLEX.
        Start symbol: main. *)

  | MenhirState044 : (('s, _menhir_box_main) _menhir_cell1_fun_ty_list, _menhir_box_main) _menhir_state
    (** State 044.
        Stack shape : fun_ty_list.
        Start symbol: main. *)

  | MenhirState049 : ((('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_stype, _menhir_box_main) _menhir_state
    (** State 049.
        Stack shape : L_PAR stype.
        Start symbol: main. *)

  | MenhirState051 : ((('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_stype, _menhir_box_main) _menhir_state
    (** State 051.
        Stack shape : L_PAR stype.
        Start symbol: main. *)

  | MenhirState054 : ((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_stype, _menhir_box_main) _menhir_state
    (** State 054.
        Stack shape : VAR stype.
        Start symbol: main. *)

  | MenhirState061 : ((('s, _menhir_box_main) _menhir_cell1_L_BRACE, _menhir_box_main) _menhir_cell1_stype, _menhir_box_main) _menhir_state
    (** State 061.
        Stack shape : L_BRACE stype.
        Start symbol: main. *)

  | MenhirState063 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 063.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState065 : ((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_stype, _menhir_box_main) _menhir_state
    (** State 065.
        Stack shape : VAR stype.
        Start symbol: main. *)

  | MenhirState069 : ((('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_state
    (** State 069.
        Stack shape : L_PAR ty.
        Start symbol: main. *)

  | MenhirState070 : ((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_state
    (** State 070.
        Stack shape : VAR ty.
        Start symbol: main. *)

  | MenhirState073 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 073.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState076 : (('s, _menhir_box_main) _menhir_cell1_WAIT _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 076.
        Stack shape : WAIT VAR.
        Start symbol: main. *)

  | MenhirState080 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 080.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState084 : (('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_state
    (** State 084.
        Stack shape : L_PAR.
        Start symbol: main. *)

  | MenhirState085 : ((('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_state
    (** State 085.
        Stack shape : L_PAR L_PAR.
        Start symbol: main. *)

  | MenhirState088 : (('s, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 088.
        Stack shape : LET VAR.
        Start symbol: main. *)

  | MenhirState090 : (('s, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_state
    (** State 090.
        Stack shape : IF.
        Start symbol: main. *)

  | MenhirState091 : (('s, _menhir_box_main) _menhir_cell1_FUN, _menhir_box_main) _menhir_state
    (** State 091.
        Stack shape : FUN.
        Start symbol: main. *)

  | MenhirState093 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 093.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState095 : (('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_state
    (** State 095.
        Stack shape : L_PAR.
        Start symbol: main. *)

  | MenhirState097 : ((('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_nonempty_list_VAR_, _menhir_box_main) _menhir_state
    (** State 097.
        Stack shape : L_PAR nonempty_list(VAR).
        Start symbol: main. *)

  | MenhirState101 : ((('s, _menhir_box_main) _menhir_cell1_FUN, _menhir_box_main) _menhir_cell1_nonempty_list_fun_binder_, _menhir_box_main) _menhir_state
    (** State 101.
        Stack shape : FUN nonempty_list(fun_binder).
        Start symbol: main. *)

  | MenhirState103 : (('s, _menhir_box_main) _menhir_cell1_uop, _menhir_box_main) _menhir_state
    (** State 103.
        Stack shape : uop.
        Start symbol: main. *)

  | MenhirState105 : (('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_state
    (** State 105.
        Stack shape : L_PAR.
        Start symbol: main. *)

  | MenhirState106 : (('s, _menhir_box_main) _menhir_cell1_simple_exp, _menhir_box_main) _menhir_state
    (** State 106.
        Stack shape : simple_exp.
        Start symbol: main. *)

  | MenhirState107 : ((('s, _menhir_box_main) _menhir_cell1_simple_exp, _menhir_box_main) _menhir_cell1_QUESTION, _menhir_box_main) _menhir_state
    (** State 107.
        Stack shape : simple_exp QUESTION.
        Start symbol: main. *)

  | MenhirState109 : (((('s, _menhir_box_main) _menhir_cell1_simple_exp, _menhir_box_main) _menhir_cell1_QUESTION, _menhir_box_main) _menhir_cell1_simple_exp, _menhir_box_main) _menhir_state
    (** State 109.
        Stack shape : simple_exp QUESTION simple_exp.
        Start symbol: main. *)

  | MenhirState111 : ((('s, _menhir_box_main) _menhir_cell1_simple_exp, _menhir_box_main) _menhir_cell1_simple_exp, _menhir_box_main) _menhir_state
    (** State 111.
        Stack shape : simple_exp simple_exp.
        Start symbol: main. *)

  | MenhirState117 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 117.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState119 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 119.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState121 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 121.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState126 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 126.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState128 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 128.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState130 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 130.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState132 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 132.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState134 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 134.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState136 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 136.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState141 : (('s, _menhir_box_main) _menhir_cell1_fun_binder, _menhir_box_main) _menhir_state
    (** State 141.
        Stack shape : fun_binder.
        Start symbol: main. *)

  | MenhirState144 : ((('s, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 144.
        Stack shape : IF expression.
        Start symbol: main. *)

  | MenhirState146 : (((('s, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 146.
        Stack shape : IF expression expression.
        Start symbol: main. *)

  | MenhirState150 : ((('s, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 150.
        Stack shape : LET VAR expression.
        Start symbol: main. *)

  | MenhirState153 : (((('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_state
    (** State 153.
        Stack shape : L_PAR L_PAR ty.
        Start symbol: main. *)

  | MenhirState156 : ((('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_state
    (** State 156.
        Stack shape : L_PAR ty.
        Start symbol: main. *)

  | MenhirState158 : ((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 158.
        Stack shape : VAR expression.
        Start symbol: main. *)

  | MenhirState159 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 159.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState162 : (((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_cell1_list_VAR_, _menhir_box_main) _menhir_state
    (** State 162.
        Stack shape : VAR expression list(VAR).
        Start symbol: main. *)

  | MenhirState166 : (('s, _menhir_box_main) _menhir_cell1_SEND_CHAN _menhir_cell0_VAR _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 166.
        Stack shape : SEND_CHAN VAR VAR.
        Start symbol: main. *)

  | MenhirState168 : (('s, _menhir_box_main) _menhir_cell1_SEND _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 168.
        Stack shape : SEND VAR.
        Start symbol: main. *)

  | MenhirState170 : ((('s, _menhir_box_main) _menhir_cell1_SEND _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 170.
        Stack shape : SEND VAR expression.
        Start symbol: main. *)

  | MenhirState171 : (('s, _menhir_box_main) _menhir_cell1_PRINT, _menhir_box_main) _menhir_state
    (** State 171.
        Stack shape : PRINT.
        Start symbol: main. *)

  | MenhirState173 : ((('s, _menhir_box_main) _menhir_cell1_PRINT, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 173.
        Stack shape : PRINT expression.
        Start symbol: main. *)

  | MenhirState174 : (('s, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_state
    (** State 174.
        Stack shape : IF.
        Start symbol: main. *)

  | MenhirState176 : ((('s, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 176.
        Stack shape : IF expression.
        Start symbol: main. *)

  | MenhirState184 : (('s, _menhir_box_main) _menhir_cell1_CASE _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 184.
        Stack shape : CASE VAR.
        Start symbol: main. *)

  | MenhirState187 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 187.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState189 : ((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_proc, _menhir_box_main) _menhir_state
    (** State 189.
        Stack shape : VAR proc.
        Start symbol: main. *)

  | MenhirState193 : (((('s, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_cell1_proc, _menhir_box_main) _menhir_state
    (** State 193.
        Stack shape : IF expression proc.
        Start symbol: main. *)

  | MenhirState199 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 199.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState201 : ((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_proc, _menhir_box_main) _menhir_state
    (** State 201.
        Stack shape : VAR proc.
        Start symbol: main. *)

  | MenhirState203 : (((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_proc, _menhir_box_main) _menhir_cell1_list_VAR_, _menhir_box_main) _menhir_state
    (** State 203.
        Stack shape : VAR proc list(VAR).
        Start symbol: main. *)

  | MenhirState207 : (('s, _menhir_box_main) _menhir_cell1_VAR _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 207.
        Stack shape : VAR VAR.
        Start symbol: main. *)

  | MenhirState211 : (('s, _menhir_box_main) _menhir_cell1_VAR _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 211.
        Stack shape : VAR VAR.
        Start symbol: main. *)

  | MenhirState215 : (('s, _menhir_box_main) _menhir_cell1_VAR _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 215.
        Stack shape : VAR VAR.
        Start symbol: main. *)

  | MenhirState217 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 217.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState218 : ((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_state
    (** State 218.
        Stack shape : VAR ty.
        Start symbol: main. *)

  | MenhirState222 : (((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_cell1_LEFT_ARROW _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 222.
        Stack shape : VAR ty LEFT_ARROW VAR.
        Start symbol: main. *)

  | MenhirState228 : ((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_stype _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 228.
        Stack shape : VAR stype VAR.
        Start symbol: main. *)

  | MenhirState236 : (('s, _menhir_box_main) _menhir_cell1_TYPE _menhir_cell0_T_VAR, _menhir_box_main) _menhir_state
    (** State 236.
        Stack shape : TYPE T_VAR.
        Start symbol: main. *)

  | MenhirState240 : (('s, _menhir_box_main) _menhir_cell1_STYPE _menhir_cell0_S_VAR, _menhir_box_main) _menhir_state
    (** State 240.
        Stack shape : STYPE S_VAR.
        Start symbol: main. *)

  | MenhirState246 : ((('s _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_list_imprt_, _menhir_box_main) _menhir_cell1_list_declaration_, _menhir_box_main) _menhir_state
    (** State 246.
        Stack shape : VAR list(imprt) list(declaration).
        Start symbol: main. *)

  | MenhirState251 : (('s, _menhir_box_main) _menhir_cell1_declaration, _menhir_box_main) _menhir_state
    (** State 251.
        Stack shape : declaration.
        Start symbol: main. *)

  | MenhirState253 : (('s, _menhir_box_main) _menhir_cell1_imprt, _menhir_box_main) _menhir_state
    (** State 253.
        Stack shape : imprt.
        Start symbol: main. *)


and ('s, 'r) _menhir_cell1_declaration = 
  | MenhirCell1_declaration of 's * ('s, 'r) _menhir_state * (InputSyntax.decl)

and ('s, 'r) _menhir_cell1_expression = 
  | MenhirCell1_expression of 's * ('s, 'r) _menhir_state * (InputSyntax.exp)

and ('s, 'r) _menhir_cell1_fun_binder = 
  | MenhirCell1_fun_binder of 's * ('s, 'r) _menhir_state * (string list * InputSyntax.ty option)

and ('s, 'r) _menhir_cell1_fun_ty_list = 
  | MenhirCell1_fun_ty_list of 's * ('s, 'r) _menhir_state * (InputSyntax.ty list)

and ('s, 'r) _menhir_cell1_imprt = 
  | MenhirCell1_imprt of 's * ('s, 'r) _menhir_state * (InputSyntax.imprt)

and ('s, 'r) _menhir_cell1_list_VAR_ = 
  | MenhirCell1_list_VAR_ of 's * ('s, 'r) _menhir_state * (string list)

and ('s, 'r) _menhir_cell1_list_declaration_ = 
  | MenhirCell1_list_declaration_ of 's * ('s, 'r) _menhir_state * (InputSyntax.decl list)

and ('s, 'r) _menhir_cell1_list_imprt_ = 
  | MenhirCell1_list_imprt_ of 's * ('s, 'r) _menhir_state * (InputSyntax.imprt list)

and ('s, 'r) _menhir_cell1_nonempty_list_VAR_ = 
  | MenhirCell1_nonempty_list_VAR_ of 's * ('s, 'r) _menhir_state * (string list)

and ('s, 'r) _menhir_cell1_nonempty_list_fun_binder_ = 
  | MenhirCell1_nonempty_list_fun_binder_ of 's * ('s, 'r) _menhir_state * ((string list * InputSyntax.ty option) list)

and ('s, 'r) _menhir_cell1_proc = 
  | MenhirCell1_proc of 's * ('s, 'r) _menhir_state * (InputSyntax.proc)

and ('s, 'r) _menhir_cell1_simple_exp = 
  | MenhirCell1_simple_exp of 's * ('s, 'r) _menhir_state * (InputSyntax.exp)

and ('s, 'r) _menhir_cell1_stype = 
  | MenhirCell1_stype of 's * ('s, 'r) _menhir_state * (InputSyntax.stype)

and ('s, 'r) _menhir_cell1_ty = 
  | MenhirCell1_ty of 's * ('s, 'r) _menhir_state * (InputSyntax.ty)

and ('s, 'r) _menhir_cell1_uop = 
  | MenhirCell1_uop of 's * ('s, 'r) _menhir_state * (InputSyntax.uop)

and ('s, 'r) _menhir_cell1_AMPERSAND = 
  | MenhirCell1_AMPERSAND of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_CASE = 
  | MenhirCell1_CASE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_CIRCUMFLEX = 
  | MenhirCell1_CIRCUMFLEX of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_FUN = 
  | MenhirCell1_FUN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_IMPORT = 
  | MenhirCell1_IMPORT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LEFT_ARROW = 
  | MenhirCell1_LEFT_ARROW of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LET = 
  | MenhirCell1_LET of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_L_BRACE = 
  | MenhirCell1_L_BRACE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_L_PAR = 
  | MenhirCell1_L_PAR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_PLUS = 
  | MenhirCell1_PLUS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_PRINT = 
  | MenhirCell1_PRINT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_QUESTION = 
  | MenhirCell1_QUESTION of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_REC = 
  | MenhirCell1_REC of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_RIGHT_ARROW_BOLD = 
  | MenhirCell1_RIGHT_ARROW_BOLD of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_SEND = 
  | MenhirCell1_SEND of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_SEND_CHAN = 
  | MenhirCell1_SEND_CHAN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_STYPE = 
  | MenhirCell1_STYPE of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_S_VAR = 
  | MenhirCell0_S_VAR of 's * (
# 7 "lib/parser.mly"
       (string)
# 645 "lib/parser.ml"
)

and ('s, 'r) _menhir_cell1_TYPE = 
  | MenhirCell1_TYPE of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_T_VAR = 
  | MenhirCell0_T_VAR of 's * (
# 6 "lib/parser.mly"
       (string)
# 655 "lib/parser.ml"
)

and ('s, 'r) _menhir_cell1_VAR = 
  | MenhirCell1_VAR of 's * ('s, 'r) _menhir_state * (
# 5 "lib/parser.mly"
       (string)
# 662 "lib/parser.ml"
)

and 's _menhir_cell0_VAR = 
  | MenhirCell0_VAR of 's * (
# 5 "lib/parser.mly"
       (string)
# 669 "lib/parser.ml"
)

and ('s, 'r) _menhir_cell1_WAIT = 
  | MenhirCell1_WAIT of 's * ('s, 'r) _menhir_state

and _menhir_box_main = 
  | MenhirBox_main of (InputSyntax.modl) [@@unboxed]

let _menhir_action_01 =
  fun e t ->
    (
# 110 "lib/parser.mly"
                                      ( Annot (e, t) )
# 683 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_02 =
  fun e1 e2 ->
    (
# 113 "lib/parser.mly"
                                      ( FunApp (e1, e2) )
# 691 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_03 =
  fun l p ->
    (
# 102 "lib/parser.mly"
                                                       ( [(l, p)] )
# 699 "lib/parser.ml"
     : ((string * InputSyntax.proc) list))

let _menhir_action_04 =
  fun cl l p ->
    (
# 103 "lib/parser.mly"
                                                       ( (l, p)::cl )
# 707 "lib/parser.ml"
     : ((string * InputSyntax.proc) list))

let _menhir_action_05 =
  fun st v ->
    (
# 98 "lib/parser.mly"
                                                    ( [(v, st)] )
# 715 "lib/parser.ml"
     : ((string * InputSyntax.stype) list))

let _menhir_action_06 =
  fun cl st v ->
    (
# 99 "lib/parser.mly"
                                                    ( (v,st)::cl )
# 723 "lib/parser.ml"
     : ((string * InputSyntax.stype) list))

let _menhir_action_07 =
  fun cond e1 e2 ->
    (
# 106 "lib/parser.mly"
                                                                         ( Cond (cond, e1, e2) )
# 731 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_08 =
  fun cond e1 e2 ->
    (
# 107 "lib/parser.mly"
                                                                         ( Cond (cond, e1, e2) )
# 739 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_09 =
  fun st v ->
    (
# 46 "lib/parser.mly"
                                                      ( Decl (v, Annot (Var v, TProc(st, []))) )
# 747 "lib/parser.ml"
     : (InputSyntax.decl))

let _menhir_action_10 =
  fun t v ->
    (
# 47 "lib/parser.mly"
                                                      ( Decl (v, Annot (Var v, t)))
# 755 "lib/parser.ml"
     : (InputSyntax.decl))

let _menhir_action_11 =
  fun e t var ->
    (
# 48 "lib/parser.mly"
                                                      ( Decl (var, Annot (e, t)) )
# 763 "lib/parser.ml"
     : (InputSyntax.decl))

let _menhir_action_12 =
  fun e ->
    (
# 62 "lib/parser.mly"
                   ( ExecExp e )
# 771 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_13 =
  fun e ->
    (
# 63 "lib/parser.mly"
                   ( ExecExp e )
# 779 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_14 =
  fun _1 ->
    (
# 51 "lib/parser.mly"
                                                                      ( _1 )
# 787 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_15 =
  fun body l ->
    (
# 52 "lib/parser.mly"
                                                                      ( FunDef (l, body) )
# 795 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_16 =
  fun e1 e2 var ->
    (
# 53 "lib/parser.mly"
                                                                      ( Let (var, e1, e2) )
# 803 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_17 =
  fun e1 e2 ->
    let op = 
# 153 "lib/parser.mly"
              ( Mul )
# 811 "lib/parser.ml"
     in
    (
# 54 "lib/parser.mly"
                                                                      ( BOp (op, e1, e2) )
# 816 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_18 =
  fun e1 e2 ->
    let op = 
# 154 "lib/parser.mly"
              ( Div )
# 824 "lib/parser.ml"
     in
    (
# 54 "lib/parser.mly"
                                                                      ( BOp (op, e1, e2) )
# 829 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_19 =
  fun e1 e2 ->
    let op = 
# 155 "lib/parser.mly"
              ( And )
# 837 "lib/parser.ml"
     in
    (
# 54 "lib/parser.mly"
                                                                      ( BOp (op, e1, e2) )
# 842 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_20 =
  fun e1 e2 ->
    let op = 
# 156 "lib/parser.mly"
              ( Or )
# 850 "lib/parser.ml"
     in
    (
# 54 "lib/parser.mly"
                                                                      ( BOp (op, e1, e2) )
# 855 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_21 =
  fun e1 e2 ->
    let op = 
# 157 "lib/parser.mly"
              ( Add )
# 863 "lib/parser.ml"
     in
    (
# 54 "lib/parser.mly"
                                                                      ( BOp (op, e1, e2) )
# 868 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_22 =
  fun e1 e2 ->
    let op = 
# 158 "lib/parser.mly"
              ( Sub )
# 876 "lib/parser.ml"
     in
    (
# 54 "lib/parser.mly"
                                                                      ( BOp (op, e1, e2) )
# 881 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_23 =
  fun e1 e2 ->
    let op = 
# 159 "lib/parser.mly"
              ( Lesser )
# 889 "lib/parser.ml"
     in
    (
# 54 "lib/parser.mly"
                                                                      ( BOp (op, e1, e2) )
# 894 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_24 =
  fun e1 e2 ->
    let op = 
# 160 "lib/parser.mly"
              ( Greater )
# 902 "lib/parser.ml"
     in
    (
# 54 "lib/parser.mly"
                                                                      ( BOp (op, e1, e2) )
# 907 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_25 =
  fun e1 e2 ->
    let op = 
# 161 "lib/parser.mly"
              ( Equals )
# 915 "lib/parser.ml"
     in
    (
# 54 "lib/parser.mly"
                                                                      ( BOp (op, e1, e2) )
# 920 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_26 =
  fun e op ->
    (
# 55 "lib/parser.mly"
                                                                      ( UOp (op, e) )
# 928 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_27 =
  fun _1 ->
    (
# 56 "lib/parser.mly"
                                                                      ( _1 )
# 936 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_28 =
  fun _1 ->
    (
# 57 "lib/parser.mly"
                                                                      ( _1 )
# 944 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_29 =
  fun _1 ->
    (
# 58 "lib/parser.mly"
                                                                      ( _1 )
# 952 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_30 =
  fun _1 ->
    (
# 59 "lib/parser.mly"
                                                                      ( _1 )
# 960 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_31 =
  fun x ->
    (
# 123 "lib/parser.mly"
                                              ( ([x], None) )
# 968 "lib/parser.ml"
     : (string list * InputSyntax.ty option))

let _menhir_action_32 =
  fun t x ->
    (
# 124 "lib/parser.mly"
                                              ( ([x], Some t) )
# 976 "lib/parser.ml"
     : (string list * InputSyntax.ty option))

let _menhir_action_33 =
  fun lx t ->
    (
# 125 "lib/parser.mly"
                                              ( (lx, Some t) )
# 984 "lib/parser.ml"
     : (string list * InputSyntax.ty option))

let _menhir_action_34 =
  fun _1 ->
    (
# 145 "lib/parser.mly"
                                              ( [_1] )
# 992 "lib/parser.ml"
     : (InputSyntax.ty list))

let _menhir_action_35 =
  fun l t ->
    (
# 146 "lib/parser.mly"
                                              ( l@[t] )
# 1000 "lib/parser.ml"
     : (InputSyntax.ty list))

let _menhir_action_36 =
  fun i lvar ->
    (
# 39 "lib/parser.mly"
                                                           ( Imprt (i, lvar) )
# 1008 "lib/parser.ml"
     : (InputSyntax.imprt))

let _menhir_action_37 =
  fun st v ->
    (
# 141 "lib/parser.mly"
                                                 ( [(v, st)] )
# 1016 "lib/parser.ml"
     : ((string * InputSyntax.stype) list))

let _menhir_action_38 =
  fun cl st v ->
    (
# 142 "lib/parser.mly"
                                                 ( (v,st)::cl )
# 1024 "lib/parser.ml"
     : ((string * InputSyntax.stype) list))

let _menhir_action_39 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 1032 "lib/parser.ml"
     : (string list))

let _menhir_action_40 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 1040 "lib/parser.ml"
     : (string list))

let _menhir_action_41 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 1048 "lib/parser.ml"
     : (InputSyntax.decl list))

let _menhir_action_42 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 1056 "lib/parser.ml"
     : (InputSyntax.decl list))

let _menhir_action_43 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 1064 "lib/parser.ml"
     : (InputSyntax.imprt list))

let _menhir_action_44 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 1072 "lib/parser.ml"
     : (InputSyntax.imprt list))

let _menhir_action_45 =
  fun i m p ->
    (
# 36 "lib/parser.mly"
                                                       ( Modl (m, i, p) )
# 1080 "lib/parser.ml"
     : (InputSyntax.modl))

let _menhir_action_46 =
  fun x ->
    (
# 228 "<standard.mly>"
    ( [ x ] )
# 1088 "lib/parser.ml"
     : (string list))

let _menhir_action_47 =
  fun x xs ->
    (
# 231 "<standard.mly>"
    ( x :: xs )
# 1096 "lib/parser.ml"
     : (string list))

let _menhir_action_48 =
  fun x ->
    (
# 228 "<standard.mly>"
    ( [ x ] )
# 1104 "lib/parser.ml"
     : ((string list * InputSyntax.ty option) list))

let _menhir_action_49 =
  fun x xs ->
    (
# 231 "<standard.mly>"
    ( x :: xs )
# 1112 "lib/parser.ml"
     : ((string list * InputSyntax.ty option) list))

let _menhir_action_50 =
  fun x ->
    (
# 228 "<standard.mly>"
    ( [ x ] )
# 1120 "lib/parser.ml"
     : (InputSyntax.exp list))

let _menhir_action_51 =
  fun x xs ->
    (
# 231 "<standard.mly>"
    ( x :: xs )
# 1128 "lib/parser.ml"
     : (InputSyntax.exp list))

let _menhir_action_52 =
  fun c e p ->
    (
# 69 "lib/parser.mly"
                                                                                        ( Send (c, e, p) )
# 1136 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_53 =
  fun c id p t ->
    (
# 70 "lib/parser.mly"
                                                                                        ( Recv (id, c, Some t, p) )
# 1144 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_54 =
  fun c id p ->
    (
# 71 "lib/parser.mly"
                                                                                        ( Recv (id, c, None, p) )
# 1152 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_55 =
  fun c ->
    (
# 72 "lib/parser.mly"
                                                                                        ( Close (c) )
# 1160 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_56 =
  fun c p ->
    (
# 73 "lib/parser.mly"
                                                                                        ( Wait (c, p) )
# 1168 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_57 =
  fun c1 c2 ->
    (
# 74 "lib/parser.mly"
                                                                                        ( Fwd (c1, c2) )
# 1176 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_58 =
  fun args c e p ->
    (
# 75 "lib/parser.mly"
                                                                                        ( Spawn (c, e, p, args) )
# 1184 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_59 =
  fun args c p1 p2 ->
    (
# 76 "lib/parser.mly"
                                                                                        ( Spawn (c, ProcExp(c, p1), p2, args) )
# 1192 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_60 =
  fun c l ->
    (
# 77 "lib/parser.mly"
                                                                                        ( Choice (c, l) )
# 1200 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_61 =
  fun c l p ->
    (
# 78 "lib/parser.mly"
                                                                                        ( Label (c, l, p) )
# 1208 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_62 =
  fun c e p ->
    (
# 79 "lib/parser.mly"
                                                                                        ( SendChan (c, e, p) )
# 1216 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_63 =
  fun c id p st ->
    (
# 80 "lib/parser.mly"
                                                                                        ( RecvChan (id, c, Some st, p) )
# 1224 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_64 =
  fun c id p ->
    (
# 81 "lib/parser.mly"
                                                                                        ( RecvChan (id, c, None, p) )
# 1232 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_65 =
  fun e p ->
    (
# 82 "lib/parser.mly"
                                                                                        ( Print (e, p) )
# 1240 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_66 =
  fun e p1 p2 ->
    (
# 83 "lib/parser.mly"
                                                                                        ( If (e, p1, p2) )
# 1248 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_67 =
  fun c p ->
    (
# 66 "lib/parser.mly"
                                                ( ProcExp (c, p) )
# 1256 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_68 =
  fun d e ->
    (
# 43 "lib/parser.mly"
                                                ( Prog (d, e) )
# 1264 "lib/parser.ml"
     : (InputSyntax.prog))

let _menhir_action_69 =
  fun () ->
    (
# 116 "lib/parser.mly"
                                    ( UnitVal )
# 1272 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_70 =
  fun i ->
    (
# 117 "lib/parser.mly"
                                    ( Num (i) )
# 1280 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_71 =
  fun b ->
    (
# 118 "lib/parser.mly"
                                    ( Bool (b) )
# 1288 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_72 =
  fun v ->
    (
# 119 "lib/parser.mly"
                                    ( Var (v) )
# 1296 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_73 =
  fun e ->
    (
# 120 "lib/parser.mly"
                                    ( e )
# 1304 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_74 =
  fun () ->
    (
# 132 "lib/parser.mly"
                                                          ( TUnit )
# 1312 "lib/parser.ml"
     : (InputSyntax.ty))

let _menhir_action_75 =
  fun () ->
    (
# 133 "lib/parser.mly"
                                                          ( TNum )
# 1320 "lib/parser.ml"
     : (InputSyntax.ty))

let _menhir_action_76 =
  fun () ->
    (
# 134 "lib/parser.mly"
                                                          ( TBool )
# 1328 "lib/parser.ml"
     : (InputSyntax.ty))

let _menhir_action_77 =
  fun s ->
    (
# 135 "lib/parser.mly"
                                                          ( TProc (s, []))
# 1336 "lib/parser.ml"
     : (InputSyntax.ty))

let _menhir_action_78 =
  fun ctxt s ->
    (
# 136 "lib/parser.mly"
                                                          ( TProc (s, ctxt))
# 1344 "lib/parser.ml"
     : (InputSyntax.ty))

let _menhir_action_79 =
  fun t ->
    (
# 137 "lib/parser.mly"
                                                          ( t )
# 1352 "lib/parser.ml"
     : (InputSyntax.ty))

let _menhir_action_80 =
  fun x ->
    (
# 138 "lib/parser.mly"
                                                          ( TVar x )
# 1360 "lib/parser.ml"
     : (InputSyntax.ty))

let _menhir_action_81 =
  fun v ->
    (
# 86 "lib/parser.mly"
                                                 ( STVar (v) )
# 1368 "lib/parser.ml"
     : (InputSyntax.stype))

let _menhir_action_82 =
  fun sty v ->
    (
# 87 "lib/parser.mly"
                                                 ( STRec (v, sty) )
# 1376 "lib/parser.ml"
     : (InputSyntax.stype))

let _menhir_action_83 =
  fun st t ->
    (
# 88 "lib/parser.mly"
                                                 ( STSend (t, st) )
# 1384 "lib/parser.ml"
     : (InputSyntax.stype))

let _menhir_action_84 =
  fun st t ->
    (
# 89 "lib/parser.mly"
                                                 ( STRecv (t, st) )
# 1392 "lib/parser.ml"
     : (InputSyntax.stype))

let _menhir_action_85 =
  fun () ->
    (
# 90 "lib/parser.mly"
                                                 ( STEnd )
# 1400 "lib/parser.ml"
     : (InputSyntax.stype))

let _menhir_action_86 =
  fun l ->
    (
# 91 "lib/parser.mly"
                                                 ( STExtChoice (l) )
# 1408 "lib/parser.ml"
     : (InputSyntax.stype))

let _menhir_action_87 =
  fun l ->
    (
# 92 "lib/parser.mly"
                                                 ( STIntChoice (l) )
# 1416 "lib/parser.ml"
     : (InputSyntax.stype))

let _menhir_action_88 =
  fun st1 st2 ->
    (
# 93 "lib/parser.mly"
                                                 ( STSendChan (st1, st2) )
# 1424 "lib/parser.ml"
     : (InputSyntax.stype))

let _menhir_action_89 =
  fun st1 st2 ->
    (
# 94 "lib/parser.mly"
                                                 ( STRecvChan (st1, st2) )
# 1432 "lib/parser.ml"
     : (InputSyntax.stype))

let _menhir_action_90 =
  fun v ->
    (
# 95 "lib/parser.mly"
                                                 ( STUVar (v) )
# 1440 "lib/parser.ml"
     : (InputSyntax.stype))

let _menhir_action_91 =
  fun _1 ->
    (
# 128 "lib/parser.mly"
                                               ( _1 )
# 1448 "lib/parser.ml"
     : (InputSyntax.ty))

let _menhir_action_92 =
  fun l t ->
    (
# 129 "lib/parser.mly"
                                               ( TFun(l, t) )
# 1456 "lib/parser.ml"
     : (InputSyntax.ty))

let _menhir_action_93 =
  fun () ->
    (
# 149 "lib/parser.mly"
          ( Neg )
# 1464 "lib/parser.ml"
     : (InputSyntax.uop))

let _menhir_action_94 =
  fun () ->
    (
# 150 "lib/parser.mly"
          ( Not )
# 1472 "lib/parser.ml"
     : (InputSyntax.uop))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | AMPERSAND ->
        "AMPERSAND"
    | AND ->
        "AND"
    | BOOL _ ->
        "BOOL"
    | CASE ->
        "CASE"
    | CIRCUMFLEX ->
        "CIRCUMFLEX"
    | CLOSE ->
        "CLOSE"
    | COLON ->
        "COLON"
    | COMMA ->
        "COMMA"
    | DIV ->
        "DIV"
    | DOT ->
        "DOT"
    | ELSE ->
        "ELSE"
    | END ->
        "END"
    | ENDIF ->
        "ENDIF"
    | END_STYPE ->
        "END_STYPE"
    | EOF ->
        "EOF"
    | EQUALS ->
        "EQUALS"
    | FUN ->
        "FUN"
    | FWD ->
        "FWD"
    | GREATER ->
        "GREATER"
    | IF ->
        "IF"
    | IMPORT ->
        "IMPORT"
    | IN ->
        "IN"
    | INT _ ->
        "INT"
    | LEFT_ARROW ->
        "LEFT_ARROW"
    | LESSER ->
        "LESSER"
    | LET ->
        "LET"
    | LOLLIPOP ->
        "LOLLIPOP"
    | L_BRACE ->
        "L_BRACE"
    | L_PAR ->
        "L_PAR"
    | MINUS ->
        "MINUS"
    | MODULE ->
        "MODULE"
    | MULT ->
        "MULT"
    | NOT ->
        "NOT"
    | OF ->
        "OF"
    | OR ->
        "OR"
    | PLUS ->
        "PLUS"
    | PRINT ->
        "PRINT"
    | QUESTION ->
        "QUESTION"
    | REC ->
        "REC"
    | RECV ->
        "RECV"
    | RECV_CHAN ->
        "RECV_CHAN"
    | RETURN ->
        "RETURN"
    | RIGHT_ARROW ->
        "RIGHT_ARROW"
    | RIGHT_ARROW_BOLD ->
        "RIGHT_ARROW_BOLD"
    | R_BRACE ->
        "R_BRACE"
    | R_PAR ->
        "R_PAR"
    | SEMI_COLON ->
        "SEMI_COLON"
    | SEND ->
        "SEND"
    | SEND_CHAN ->
        "SEND_CHAN"
    | SPAWN ->
        "SPAWN"
    | STYPE ->
        "STYPE"
    | S_VAR _ ->
        "S_VAR"
    | TBOOL ->
        "TBOOL"
    | THEN ->
        "THEN"
    | TNUM ->
        "TNUM"
    | TUNIT ->
        "TUNIT"
    | TYPE ->
        "TYPE"
    | T_VAR _ ->
        "T_VAR"
    | UNIT_VAL ->
        "UNIT_VAL"
    | VAR _ ->
        "VAR"
    | WAIT ->
        "WAIT"
    | WHERE ->
        "WHERE"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_goto_exec_exp : type  ttv_stack. ((ttv_stack _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_list_imprt_, _menhir_box_main) _menhir_cell1_list_declaration_ -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RETURN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_list_declaration_ (_menhir_stack, _, d) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_68 d e in
          (match (_tok : MenhirBasics.token) with
          | EOF ->
              let MenhirCell1_list_imprt_ (_menhir_stack, _, i) = _menhir_stack in
              let MenhirCell0_VAR (_menhir_stack, m) = _menhir_stack in
              let p = _v in
              let _v = _menhir_action_45 i m p in
              MenhirBox_main _v
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  let _menhir_run_247 : type  ttv_stack. ((ttv_stack _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_list_imprt_, _menhir_box_main) _menhir_cell1_list_declaration_ -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_13 e in
      _menhir_goto_exec_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  let _menhir_run_248 : type  ttv_stack. ((ttv_stack _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_list_imprt_, _menhir_box_main) _menhir_cell1_list_declaration_ -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_12 e in
      _menhir_goto_exec_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  let rec _menhir_run_004 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_IMPORT (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | L_PAR ->
              let _menhir_s = MenhirState006 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_007 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState007
      | COLON | R_PAR ->
          let x = _v in
          let _v = _menhir_action_46 x in
          _menhir_goto_nonempty_list_VAR_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_nonempty_list_VAR_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState095 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState006 ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState007 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_096 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_nonempty_list_VAR_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _menhir_s = MenhirState097 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | T_VAR _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TUNIT ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TNUM ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TBOOL ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_BRACE ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_015 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let x = _v in
      let _v = _menhir_action_80 x in
      _menhir_goto_simple_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_simple_ty : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState097 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState093 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState044 ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState240 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState236 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState014 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState217 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState085 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState019 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState020 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState063 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState030 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState038 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_098 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_nonempty_list_VAR_ -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | R_PAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_nonempty_list_VAR_ (_menhir_stack, _, lx) = _menhir_stack in
          let MenhirCell1_L_PAR (_menhir_stack, _menhir_s) = _menhir_stack in
          let t = _v in
          let _v = _menhir_action_33 lx t in
          _menhir_goto_fun_binder _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_fun_binder : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          let _menhir_stack = MenhirCell1_fun_binder (_menhir_stack, _menhir_s, _v) in
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState141
      | L_PAR ->
          let _menhir_stack = MenhirCell1_fun_binder (_menhir_stack, _menhir_s, _v) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState141
      | RIGHT_ARROW ->
          let x = _v in
          let _v = _menhir_action_48 x in
          _menhir_goto_nonempty_list_fun_binder_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_092 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState093 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | T_VAR _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TUNIT ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TNUM ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TBOOL ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_BRACE ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | L_PAR | RIGHT_ARROW | VAR _ ->
          let x = _v in
          let _v = _menhir_action_31 x in
          _menhir_goto_fun_binder _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_016 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_74 () in
      _menhir_goto_simple_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_017 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_75 () in
      _menhir_goto_simple_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_018 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_76 () in
      _menhir_goto_simple_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_019 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_L_PAR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState019 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_VAR _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TUNIT ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TNUM ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TBOOL ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_BRACE ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_020 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_L_BRACE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState020 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | T_VAR _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TUNIT ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TNUM ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TBOOL ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | S_VAR _v ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REC ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_BRACE ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | END_STYPE ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMPERSAND ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_021 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let v = _v in
      let _v = _menhir_action_81 v in
      _menhir_goto_stype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_stype : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState240 ->
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState217 ->
          _menhir_run_224 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState063 ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState020 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState029 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState049 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState030 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState038 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState040 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_241 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_STYPE _menhir_cell0_S_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_COLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_S_VAR (_menhir_stack, v) = _menhir_stack in
          let MenhirCell1_STYPE (_menhir_stack, _menhir_s) = _menhir_stack in
          let st = _v in
          let _v = _menhir_action_09 st v in
          _menhir_goto_declaration _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_declaration : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_declaration (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState251
      | TYPE ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState251
      | STYPE ->
          _menhir_run_239 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState251
      | RETURN ->
          let _v_1 = _menhir_action_41 () in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | _ ->
          _eRR ()
  
  and _menhir_run_013 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _menhir_s = MenhirState014 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | T_VAR _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TUNIT ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TNUM ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TBOOL ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_BRACE ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_235 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TYPE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_VAR _v ->
          let _menhir_stack = MenhirCell0_T_VAR (_menhir_stack, _v) in
          let _menhir_s = MenhirState236 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | T_VAR _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TUNIT ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TNUM ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TBOOL ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_BRACE ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_239 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_STYPE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | S_VAR _v ->
          let _menhir_stack = MenhirCell0_S_VAR (_menhir_stack, _v) in
          let _menhir_s = MenhirState240 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | T_VAR _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TUNIT ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TNUM ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TBOOL ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | S_VAR _v ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REC ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_BRACE ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | END_STYPE ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMPERSAND ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_022 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let v = _v in
      let _v = _menhir_action_90 v in
      _menhir_goto_stype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_023 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_REC (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | DOT ->
              let _menhir_s = MenhirState025 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | T_VAR _v ->
                  _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TUNIT ->
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | TNUM ->
                  _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | TBOOL ->
                  _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | S_VAR _v ->
                  _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | REC ->
                  _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PLUS ->
                  _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | L_PAR ->
                  _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | L_BRACE ->
                  _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | END_STYPE ->
                  _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | AMPERSAND ->
                  _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_026 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PLUS (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | L_BRACE ->
          let _menhir_s = MenhirState027 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_028 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _menhir_s = MenhirState029 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | T_VAR _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TUNIT ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TNUM ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TBOOL ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | S_VAR _v ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REC ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_BRACE ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | END_STYPE ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMPERSAND ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_030 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_L_PAR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState030 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | T_VAR _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TUNIT ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TNUM ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TBOOL ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | S_VAR _v ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REC ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_BRACE ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | END_STYPE ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMPERSAND ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_031 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_85 () in
      _menhir_goto_stype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_032 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_AMPERSAND (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | L_BRACE ->
          let _menhir_s = MenhirState033 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_252 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_declaration -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_declaration (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_42 x xs in
      _menhir_goto_list_declaration_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_list_declaration_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState251 ->
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState012 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_245 : type  ttv_stack. ((ttv_stack _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_list_imprt_ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_list_declaration_ (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState246 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_071 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LEFT_ARROW ->
          let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | L_BRACE ->
              let _menhir_s = MenhirState073 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | WAIT ->
                  _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | VAR _v ->
                  _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | SEND_CHAN ->
                  _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | SEND ->
                  _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PRINT ->
                  _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IF ->
                  _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FWD ->
                  _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CLOSE ->
                  _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CASE ->
                  _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | AND | BOOL _ | DIV | ELSE | END | ENDIF | EQUALS | GREATER | IN | INT _ | LESSER | L_PAR | MINUS | MULT | OR | PLUS | QUESTION | RETURN | R_PAR | SEMI_COLON | THEN | UNIT_VAL | VAR _ ->
          let v = _v in
          let _v = _menhir_action_72 v in
          _menhir_goto_simple_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_074 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_WAIT (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SEMI_COLON ->
              let _menhir_s = MenhirState076 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | WAIT ->
                  _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | VAR _v ->
                  _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | SEND_CHAN ->
                  _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | SEND ->
                  _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PRINT ->
                  _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IF ->
                  _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FWD ->
                  _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CLOSE ->
                  _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CASE ->
                  _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_077 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LEFT_ARROW ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SPAWN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | L_PAR ->
                  let _menhir_s = MenhirState080 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | VAR _v ->
                      _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | UNIT_VAL ->
                      _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | NOT ->
                      _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | MINUS ->
                      _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | L_PAR ->
                      _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LET ->
                      _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | INT _v ->
                      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | IF ->
                      _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | FUN ->
                      _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | BOOL _v ->
                      _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | _ ->
                      _eRR ())
              | L_BRACE ->
                  let _menhir_s = MenhirState199 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | WAIT ->
                      _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | VAR _v ->
                      _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | SEND_CHAN ->
                      _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | SEND ->
                      _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | PRINT ->
                      _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | IF ->
                      _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | FWD ->
                      _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | CLOSE ->
                      _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | CASE ->
                      _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | RECV_CHAN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | SEMI_COLON ->
                      let _menhir_s = MenhirState207 in
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      (match (_tok : MenhirBasics.token) with
                      | WAIT ->
                          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | VAR _v ->
                          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                      | SEND_CHAN ->
                          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | SEND ->
                          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | PRINT ->
                          _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | IF ->
                          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | FWD ->
                          _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | CLOSE ->
                          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | CASE ->
                          _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | _ ->
                          _eRR ())
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | RECV ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | SEMI_COLON ->
                      let _menhir_s = MenhirState211 in
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      (match (_tok : MenhirBasics.token) with
                      | WAIT ->
                          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | VAR _v ->
                          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                      | SEND_CHAN ->
                          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | SEND ->
                          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | PRINT ->
                          _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | IF ->
                          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | FWD ->
                          _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | CLOSE ->
                          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | CASE ->
                          _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | _ ->
                          _eRR ())
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | DOT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | SEMI_COLON ->
                  let _menhir_s = MenhirState215 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | WAIT ->
                      _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | VAR _v ->
                      _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | SEND_CHAN ->
                      _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | SEND ->
                      _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | PRINT ->
                      _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | IF ->
                      _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | FWD ->
                      _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | CLOSE ->
                      _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | CASE ->
                      _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | COLON ->
          let _menhir_s = MenhirState217 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | T_VAR _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TUNIT ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TNUM ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TBOOL ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | S_VAR _v ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REC ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_BRACE ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | END_STYPE ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMPERSAND ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_081 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_69 () in
      _menhir_goto_simple_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_simple_exp : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState246 ->
          _menhir_run_247 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState103 ->
          _menhir_run_138 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState111 ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState106 ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState109 ->
          _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState107 ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState070 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState174 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState171 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState085 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState150 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState090 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState101 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState134 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState132 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState128 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState121 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState119 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState117 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_138 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_uop -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_uop (_menhir_stack, _menhir_s, op) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_26 e op in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState070 ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState174 ->
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState171 ->
          _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_169 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_154 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_154 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState150 ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState090 ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState101 ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState134 ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState132 ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState128 ->
          _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState126 ->
          _menhir_run_127 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState121 ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState119 ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState117 ->
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState085 ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_233 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_ty as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_COLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_ty (_menhir_stack, _, t) = _menhir_stack in
          let MenhirCell1_VAR (_menhir_stack, _menhir_s, var) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_11 e t var in
          _menhir_goto_declaration _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_117 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState117 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_082 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_94 () in
      _menhir_goto_uop _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_uop : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_uop (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState103
      | UNIT_VAL ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState103
      | L_PAR ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState103
      | INT _v_1 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState103
      | BOOL _v_2 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState103
      | _ ->
          _eRR ()
  
  and _menhir_run_104 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let v = _v in
      let _v = _menhir_action_72 v in
      _menhir_goto_simple_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_105 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_L_PAR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState105 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_083 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_93 () in
      _menhir_goto_uop _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_084 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_L_PAR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState084 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | T_VAR _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TUNIT ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TNUM ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TBOOL ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_BRACE ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_085 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR as 'stack) -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_L_PAR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState085 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | T_VAR _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TUNIT ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TNUM ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TBOOL ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_BRACE ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_086 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | EQUALS ->
              let _menhir_s = MenhirState088 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | UNIT_VAL ->
                  _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | NOT ->
                  _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MINUS ->
                  _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | L_PAR ->
                  _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LET ->
                  _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT _v ->
                  _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IF ->
                  _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FUN ->
                  _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL _v ->
                  _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_089 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let i = _v in
      let _v = _menhir_action_70 i in
      _menhir_goto_simple_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_090 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState090 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_091 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FUN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState091 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | L_PAR ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_095 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_L_PAR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState095 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_102 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let b = _v in
      let _v = _menhir_action_71 b in
      _menhir_goto_simple_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_119 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState119 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_121 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState121 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_134 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState134 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_126 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState126 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_130 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState130 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_136 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState136 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_128 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState128 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_132 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState132 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_175 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | THEN ->
          let _menhir_s = MenhirState176 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WAIT ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SEND_CHAN ->
              _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEND ->
              _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FWD ->
              _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CLOSE ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | PLUS ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_163 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_SEND_CHAN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | SEMI_COLON ->
                  let _menhir_s = MenhirState166 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | WAIT ->
                      _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | VAR _v ->
                      _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | SEND_CHAN ->
                      _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | SEND ->
                      _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | PRINT ->
                      _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | IF ->
                      _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | FWD ->
                      _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | CLOSE ->
                      _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | CASE ->
                      _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_167 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_SEND (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
          let _menhir_s = MenhirState168 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT_VAL ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL _v ->
              _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_171 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PRINT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState171 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_174 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState174 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_177 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let (c2, c1) = (_v_0, _v) in
              let _v = _menhir_action_57 c1 c2 in
              _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_proc : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState073 ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState076 ->
          _menhir_run_230 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState228 ->
          _menhir_run_229 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState222 ->
          _menhir_run_223 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState215 ->
          _menhir_run_216 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState211 ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState207 ->
          _menhir_run_208 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState203 ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState199 ->
          _menhir_run_200 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState162 ->
          _menhir_run_198 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState166 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState170 ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState173 ->
          _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState193 ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState176 ->
          _menhir_run_192 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState187 ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_231 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | R_BRACE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_VAR (_menhir_stack, _menhir_s, c) = _menhir_stack in
          let p = _v in
          let _v = _menhir_action_67 c p in
          _menhir_goto_proc_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_proc_exp : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState246 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState070 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState174 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState171 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState085 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState150 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState090 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState101 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState134 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState132 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState128 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState121 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState119 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState117 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_114 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_30 _1 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_230 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_WAIT _menhir_cell0_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_VAR (_menhir_stack, c) = _menhir_stack in
      let MenhirCell1_WAIT (_menhir_stack, _menhir_s) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_56 c p in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_229 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_stype _menhir_cell0_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_VAR (_menhir_stack, c) = _menhir_stack in
      let MenhirCell1_stype (_menhir_stack, _, st) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, id) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_63 c id p st in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_223 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_cell1_LEFT_ARROW _menhir_cell0_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_VAR (_menhir_stack, c) = _menhir_stack in
      let MenhirCell1_LEFT_ARROW (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_ty (_menhir_stack, _, t) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, id) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_53 c id p t in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_216 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_VAR _menhir_cell0_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_VAR (_menhir_stack, l) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, c) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_61 c l p in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_212 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_VAR _menhir_cell0_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_VAR (_menhir_stack, c) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, id) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_54 c id p in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_208 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_VAR _menhir_cell0_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_VAR (_menhir_stack, c) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, id) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_64 c id p in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_204 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_proc, _menhir_box_main) _menhir_cell1_list_VAR_ -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_list_VAR_ (_menhir_stack, _, args) = _menhir_stack in
      let MenhirCell1_proc (_menhir_stack, _, p1) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, c) = _menhir_stack in
      let p2 = _v in
      let _v = _menhir_action_59 args c p1 p2 in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_200 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_proc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | R_BRACE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v_0 ->
              _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState201
          | SEMI_COLON ->
              let _v_1 = _menhir_action_39 () in
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState201
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_159 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState159
      | SEMI_COLON ->
          let _v_1 = _menhir_action_39 () in
          _menhir_run_160 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | _ ->
          _eRR ()
  
  and _menhir_run_160 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_VAR -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_40 x xs in
      _menhir_goto_list_VAR_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_list_VAR_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState201 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState158 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState159 ->
          _menhir_run_160 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_202 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_proc as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_list_VAR_ (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState203 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WAIT ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VAR _v ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SEND_CHAN ->
          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SEND ->
          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FWD ->
          _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CLOSE ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CASE ->
          _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_180 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v in
          let _v = _menhir_action_55 c in
          _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_182 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_CASE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | OF ->
              let _menhir_s = MenhirState184 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_185 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_185 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | L_PAR ->
              let _menhir_s = MenhirState187 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | WAIT ->
                  _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | VAR _v ->
                  _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | SEND_CHAN ->
                  _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | SEND ->
                  _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PRINT ->
                  _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IF ->
                  _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FWD ->
                  _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CLOSE ->
                  _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CASE ->
                  _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_161 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_list_VAR_ (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState162 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WAIT ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VAR _v ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SEND_CHAN ->
          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SEND ->
          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FWD ->
          _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CLOSE ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CASE ->
          _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_198 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_cell1_list_VAR_ -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_list_VAR_ (_menhir_stack, _, args) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, e) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, c) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_58 args c e p in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_197 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_SEND_CHAN _menhir_cell0_VAR _menhir_cell0_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_VAR (_menhir_stack, e) = _menhir_stack in
      let MenhirCell0_VAR (_menhir_stack, c) = _menhir_stack in
      let MenhirCell1_SEND_CHAN (_menhir_stack, _menhir_s) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_62 c e p in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_196 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_SEND _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expression (_menhir_stack, _, e) = _menhir_stack in
      let MenhirCell0_VAR (_menhir_stack, c) = _menhir_stack in
      let MenhirCell1_SEND (_menhir_stack, _menhir_s) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_52 c e p in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_195 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_PRINT, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expression (_menhir_stack, _, e) = _menhir_stack in
      let MenhirCell1_PRINT (_menhir_stack, _menhir_s) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_65 e p in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_194 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_cell1_proc -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_proc (_menhir_stack, _, p1) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, e) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
      let p2 = _v in
      let _v = _menhir_action_66 e p1 p2 in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_192 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_proc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ELSE ->
          let _menhir_s = MenhirState193 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WAIT ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SEND_CHAN ->
              _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEND ->
              _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FWD ->
              _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CLOSE ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_188 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | R_PAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v_0 ->
              let _menhir_stack = MenhirCell1_proc (_menhir_stack, _menhir_s, _v) in
              _menhir_run_185 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState189
          | ELSE | R_BRACE | R_PAR ->
              let MenhirCell1_VAR (_menhir_stack, _menhir_s, l) = _menhir_stack in
              let p = _v in
              let _v = _menhir_action_03 l p in
              _menhir_goto_case_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_case_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState184 ->
          _menhir_run_191 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState189 ->
          _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_191 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_CASE _menhir_cell0_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_VAR (_menhir_stack, c) = _menhir_stack in
      let MenhirCell1_CASE (_menhir_stack, _menhir_s) = _menhir_stack in
      let l = _v in
      let _v = _menhir_action_60 c l in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_190 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_proc -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_proc (_menhir_stack, _, p) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, l) = _menhir_stack in
      let cl = _v in
      let _v = _menhir_action_04 cl l p in
      _menhir_goto_case_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_172 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_PRINT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMI_COLON ->
          let _menhir_s = MenhirState173 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WAIT ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SEND_CHAN ->
              _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEND ->
              _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FWD ->
              _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CLOSE ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | PLUS ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_169 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_SEND _menhir_cell0_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMI_COLON ->
          let _menhir_s = MenhirState170 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WAIT ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SEND_CHAN ->
              _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEND ->
              _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FWD ->
              _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CLOSE ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | PLUS ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_157 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | R_PAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v_0 ->
              _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState158
          | SEMI_COLON ->
              let _v_1 = _menhir_action_39 () in
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState158
          | _ ->
              _eRR ())
      | PLUS ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_154 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_ty as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | R_PAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_ty (_menhir_stack, _, t) = _menhir_stack in
          let MenhirCell1_L_PAR (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_01 e t in
          let _1 = _v in
          let _v = _menhir_action_28 _1 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_151 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer
      | END ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_expression (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell0_VAR (_menhir_stack, var) = _menhir_stack in
          let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_16 e1 e2 var in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_149 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IN ->
          let _menhir_s = MenhirState150 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT_VAL ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL _v ->
              _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | GREATER ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_147 : type  ttv_stack. ((((ttv_stack, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ENDIF ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_expression (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_expression (_menhir_stack, _, cond) = _menhir_stack in
          let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_07 cond e1 e2 in
          _menhir_goto_cond_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_goto_cond_exp : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_29 _1 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_145 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE ->
          let _menhir_s = MenhirState146 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT_VAL ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL _v ->
              _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | DIV ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_143 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | THEN ->
          let _menhir_s = MenhirState144 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT_VAL ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL _v ->
              _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | PLUS ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_139 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_FUN, _menhir_box_main) _menhir_cell1_nonempty_list_fun_binder_ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer
      | END ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_nonempty_list_fun_binder_ (_menhir_stack, _, l) = _menhir_stack in
          let MenhirCell1_FUN (_menhir_stack, _menhir_s) = _menhir_stack in
          let body = _v in
          let _v = _menhir_action_15 body l in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_137 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE | END | ENDIF | EQUALS | IN | R_PAR | SEMI_COLON | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_25 e1 e2 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_135 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE | END | ENDIF | EQUALS | IN | MINUS | PLUS | R_PAR | SEMI_COLON | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_22 e1 e2 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_133 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ELSE | END | ENDIF | EQUALS | IN | MINUS | OR | PLUS | R_PAR | SEMI_COLON | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_19 e1 e2 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_131 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ELSE | END | ENDIF | EQUALS | GREATER | IN | LESSER | MINUS | OR | PLUS | R_PAR | SEMI_COLON | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_24 e1 e2 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_129 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_18 e1 e2 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_127 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ELSE | END | ENDIF | EQUALS | GREATER | IN | LESSER | MINUS | OR | PLUS | R_PAR | SEMI_COLON | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_23 e1 e2 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_122 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_17 e1 e2 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_120 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ELSE | END | ENDIF | EQUALS | IN | MINUS | OR | PLUS | R_PAR | SEMI_COLON | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_20 e1 e2 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_118 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE | END | ENDIF | EQUALS | IN | MINUS | PLUS | R_PAR | SEMI_COLON | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_21 e1 e2 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_115 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | R_PAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_L_PAR (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_73 e in
          _menhir_goto_simple_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_111 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_simple_exp as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState111
      | UNIT_VAL ->
          let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState111
      | L_PAR ->
          let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState111
      | INT _v_1 ->
          let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState111
      | BOOL _v_2 ->
          let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState111
      | AND | DIV | ELSE | END | ENDIF | EQUALS | GREATER | IN | LESSER | MINUS | MULT | OR | PLUS | R_PAR | SEMI_COLON | THEN ->
          let x = _v in
          let _v = _menhir_action_50 x in
          _menhir_goto_nonempty_list_simple_exp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_nonempty_list_simple_exp_ : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_simple_exp as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState106 ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState111 ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_113 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_simple_exp -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_simple_exp (_menhir_stack, _menhir_s, e1) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_02 e1 e2 in
      let _1 = _v in
      let _v = _menhir_action_27 _1 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_112 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_simple_exp, _menhir_box_main) _menhir_cell1_simple_exp -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_simple_exp (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_51 x xs in
      _menhir_goto_nonempty_list_simple_exp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_110 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_simple_exp, _menhir_box_main) _menhir_cell1_QUESTION, _menhir_box_main) _menhir_cell1_simple_exp -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_simple_exp (_menhir_stack, _, e1) = _menhir_stack in
      let MenhirCell1_QUESTION (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_simple_exp (_menhir_stack, _menhir_s, cond) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_08 cond e1 e2 in
      _menhir_goto_cond_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_108 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_simple_exp, _menhir_box_main) _menhir_cell1_QUESTION as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _menhir_s = MenhirState109 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT_VAL ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_106 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState106
      | UNIT_VAL ->
          let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState106
      | QUESTION ->
          let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
          let _menhir_stack = MenhirCell1_QUESTION (_menhir_stack, MenhirState106) in
          let _menhir_s = MenhirState107 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT_VAL ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | L_PAR ->
          let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState106
      | INT _v_4 ->
          let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState106
      | BOOL _v_5 ->
          let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState106
      | AND | DIV | ELSE | END | ENDIF | EQUALS | GREATER | IN | LESSER | MINUS | MULT | OR | PLUS | R_PAR | SEMI_COLON | THEN ->
          let _1 = _v in
          let _v = _menhir_action_14 _1 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_224 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_stype (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LEFT_ARROW ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | RECV_CHAN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | SEMI_COLON ->
                      let _menhir_s = MenhirState228 in
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      (match (_tok : MenhirBasics.token) with
                      | WAIT ->
                          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | VAR _v ->
                          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                      | SEND_CHAN ->
                          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | SEND ->
                          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | PRINT ->
                          _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | IF ->
                          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | FWD ->
                          _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | CLOSE ->
                          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | CASE ->
                          _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | _ ->
                          _eRR ())
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_064 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_stype (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState065 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | R_BRACE ->
          let MenhirCell1_VAR (_menhir_stack, _menhir_s, v) = _menhir_stack in
          let st = _v in
          let _v = _menhir_action_37 st v in
          _menhir_goto_lin_ctxt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_062 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _menhir_s = MenhirState063 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | T_VAR _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TUNIT ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TNUM ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TBOOL ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | S_VAR _v ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REC ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_BRACE ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | END_STYPE ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMPERSAND ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_lin_ctxt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState061 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState065 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_067 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_BRACE, _menhir_box_main) _menhir_cell1_stype -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_stype (_menhir_stack, _, s) = _menhir_stack in
      let MenhirCell1_L_BRACE (_menhir_stack, _menhir_s) = _menhir_stack in
      let ctxt = _v in
      let _v = _menhir_action_78 ctxt s in
      _menhir_goto_simple_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_066 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_stype -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_stype (_menhir_stack, _, st) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, v) = _menhir_stack in
      let cl = _v in
      let _v = _menhir_action_38 cl st v in
      _menhir_goto_lin_ctxt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_059 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_BRACE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | R_BRACE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_L_BRACE (_menhir_stack, _menhir_s) = _menhir_stack in
          let s = _v in
          let _v = _menhir_action_77 s in
          _menhir_goto_simple_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | LEFT_ARROW ->
          let _menhir_stack = MenhirCell1_stype (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState061 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_058 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_REC _menhir_cell0_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_VAR (_menhir_stack, v) = _menhir_stack in
      let MenhirCell1_REC (_menhir_stack, _menhir_s) = _menhir_stack in
      let sty = _v in
      let _v = _menhir_action_82 sty v in
      _menhir_goto_stype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_053 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_stype (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState054 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | R_BRACE ->
          let MenhirCell1_VAR (_menhir_stack, _menhir_s, v) = _menhir_stack in
          let st = _v in
          let _v = _menhir_action_05 st v in
          _menhir_goto_choice_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_choice_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState027 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState054 ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState033 ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_056 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_PLUS -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_PLUS (_menhir_stack, _menhir_s) = _menhir_stack in
      let l = _v in
      let _v = _menhir_action_87 l in
      _menhir_goto_stype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_055 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_stype -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_stype (_menhir_stack, _, st) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, v) = _menhir_stack in
      let cl = _v in
      let _v = _menhir_action_06 cl st v in
      _menhir_goto_choice_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_034 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_AMPERSAND -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_AMPERSAND (_menhir_stack, _menhir_s) = _menhir_stack in
      let l = _v in
      let _v = _menhir_action_86 l in
      _menhir_goto_stype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_052 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_stype -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_stype (_menhir_stack, _, st1) = _menhir_stack in
      let MenhirCell1_L_PAR (_menhir_stack, _menhir_s) = _menhir_stack in
      let st2 = _v in
      let _v = _menhir_action_89 st1 st2 in
      _menhir_goto_stype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_050 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_stype -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_stype (_menhir_stack, _, st1) = _menhir_stack in
      let MenhirCell1_L_PAR (_menhir_stack, _menhir_s) = _menhir_stack in
      let st2 = _v in
      let _v = _menhir_action_88 st1 st2 in
      _menhir_goto_stype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_047 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_stype (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | R_PAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | MULT ->
              let _menhir_s = MenhirState049 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | T_VAR _v ->
                  _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TUNIT ->
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | TNUM ->
                  _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | TBOOL ->
                  _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | S_VAR _v ->
                  _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | REC ->
                  _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PLUS ->
                  _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | L_PAR ->
                  _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | L_BRACE ->
                  _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | END_STYPE ->
                  _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | AMPERSAND ->
                  _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | LOLLIPOP ->
              let _menhir_s = MenhirState051 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | T_VAR _v ->
                  _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TUNIT ->
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | TNUM ->
                  _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | TBOOL ->
                  _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | S_VAR _v ->
                  _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | REC ->
                  _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PLUS ->
                  _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | L_PAR ->
                  _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | L_BRACE ->
                  _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | END_STYPE ->
                  _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | AMPERSAND ->
                  _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_046 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_cell1_RIGHT_ARROW_BOLD -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_RIGHT_ARROW_BOLD (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_ty (_menhir_stack, _menhir_s, t) = _menhir_stack in
      let st = _v in
      let _v = _menhir_action_84 st t in
      _menhir_goto_stype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_041 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_cell1_CIRCUMFLEX -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_CIRCUMFLEX (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_ty (_menhir_stack, _menhir_s, t) = _menhir_stack in
      let st = _v in
      let _v = _menhir_action_83 st t in
      _menhir_goto_stype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_nonempty_list_fun_binder_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState141 ->
          _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState091 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_142 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_fun_binder -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_fun_binder (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_49 x xs in
      _menhir_goto_nonempty_list_fun_binder_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_100 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_FUN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_nonempty_list_fun_binder_ (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState101 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_094 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let t = _v in
      let _v = _menhir_action_32 t x in
      _menhir_goto_fun_binder _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_045 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_fun_ty_list -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RIGHT_ARROW ->
          let MenhirCell1_fun_ty_list (_menhir_stack, _menhir_s, l) = _menhir_stack in
          let t = _v in
          let _v = _menhir_action_35 l t in
          _menhir_goto_fun_ty_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _ | CIRCUMFLEX | FUN | IF | INT _ | LEFT_ARROW | LET | L_PAR | MINUS | NOT | RIGHT_ARROW_BOLD | R_PAR | SEMI_COLON | UNIT_VAL | VAR _ ->
          let MenhirCell1_fun_ty_list (_menhir_stack, _menhir_s, l) = _menhir_stack in
          let t = _v in
          let _v = _menhir_action_92 l t in
          _menhir_goto_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_fun_ty_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_fun_ty_list (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState044 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_VAR _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TUNIT ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TNUM ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TBOOL ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_BRACE ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_ty : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState236 ->
          _menhir_run_237 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState217 ->
          _menhir_run_218 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_156 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState085 ->
          _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState014 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState019 ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState240 ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState063 ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState020 ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState038 ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState030 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_237 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_TYPE _menhir_cell0_T_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_COLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_T_VAR (_menhir_stack, v) = _menhir_stack in
          let MenhirCell1_TYPE (_menhir_stack, _menhir_s) = _menhir_stack in
          let t = _v in
          let _v = _menhir_action_10 t v in
          _menhir_goto_declaration _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_218 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RIGHT_ARROW_BOLD ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState218
      | LEFT_ARROW ->
          let _menhir_stack = MenhirCell1_LEFT_ARROW (_menhir_stack, MenhirState218) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | RECV ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | SEMI_COLON ->
                      let _menhir_s = MenhirState222 in
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      (match (_tok : MenhirBasics.token) with
                      | WAIT ->
                          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | VAR _v ->
                          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                      | SEND_CHAN ->
                          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | SEND ->
                          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | PRINT ->
                          _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | IF ->
                          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | FWD ->
                          _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | CLOSE ->
                          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | CASE ->
                          _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | _ ->
                          _eRR ())
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | CIRCUMFLEX ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState218
      | _ ->
          _eRR ()
  
  and _menhir_run_038 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_ty as 'stack) -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_RIGHT_ARROW_BOLD (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState038 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | T_VAR _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TUNIT ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TNUM ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TBOOL ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | S_VAR _v ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REC ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_BRACE ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | END_STYPE ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMPERSAND ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_040 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_ty as 'stack) -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_CIRCUMFLEX (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState040 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | T_VAR _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TUNIT ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TNUM ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TBOOL ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | S_VAR _v ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REC ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_BRACE ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | END_STYPE ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMPERSAND ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_156 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState156
      | UNIT_VAL ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | NOT ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | MINUS ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | L_PAR ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | LET ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | INT _v_1 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState156
      | IF ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | FUN ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | BOOL _v_2 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState156
      | _ ->
          _eRR ()
  
  and _menhir_run_153 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_L_PAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState153
      | UNIT_VAL ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | R_PAR ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NOT ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | MINUS ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | L_PAR ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | LET ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | INT _v_1 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState153
      | IF ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | FUN ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | BOOL _v_2 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState153
      | _ ->
          _eRR ()
  
  and _menhir_run_037 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_ty -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_ty (_menhir_stack, _, t) = _menhir_stack in
      let MenhirCell1_L_PAR (_menhir_stack, _menhir_s) = _menhir_stack in
      let _v = _menhir_action_79 t in
      _menhir_goto_simple_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_070 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState070
      | UNIT_VAL ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | NOT ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | MINUS ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | L_PAR ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | LET ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | INT _v_1 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState070
      | IF ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | FUN ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | BOOL _v_2 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState070
      | _ ->
          _eRR ()
  
  and _menhir_run_069 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | R_PAR ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_039 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RIGHT_ARROW_BOLD ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState039
      | CIRCUMFLEX ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState039
      | _ ->
          _eRR ()
  
  and _menhir_run_036 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | R_PAR ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RIGHT_ARROW_BOLD ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState036
      | CIRCUMFLEX ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState036
      | _ ->
          _eRR ()
  
  and _menhir_run_042 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RIGHT_ARROW ->
          let _1 = _v in
          let _v = _menhir_action_34 _1 in
          _menhir_goto_fun_ty_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _ | CIRCUMFLEX | FUN | IF | INT _ | LEFT_ARROW | LET | L_PAR | MINUS | NOT | RIGHT_ARROW_BOLD | R_PAR | SEMI_COLON | UNIT_VAL | VAR _ ->
          let _1 = _v in
          let _v = _menhir_action_91 _1 in
          _menhir_goto_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_009 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_IMPORT _menhir_cell0_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | R_PAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | END ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let MenhirCell0_VAR (_menhir_stack, i) = _menhir_stack in
              let MenhirCell1_IMPORT (_menhir_stack, _menhir_s) = _menhir_stack in
              let lvar = _v in
              let _v = _menhir_action_36 i lvar in
              let _menhir_stack = MenhirCell1_imprt (_menhir_stack, _menhir_s, _v) in
              (match (_tok : MenhirBasics.token) with
              | IMPORT ->
                  _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState253
              | RETURN | STYPE | TYPE | VAR _ ->
                  let _v_0 = _menhir_action_43 () in
                  _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_254 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_imprt -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_imprt (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_44 x xs in
      _menhir_goto_list_imprt_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_list_imprt_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState253 ->
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState003 ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_012 : type  ttv_stack. (ttv_stack _menhir_cell0_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_list_imprt_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState012
      | TYPE ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState012
      | STYPE ->
          _menhir_run_239 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState012
      | RETURN ->
          let _v_1 = _menhir_action_41 () in
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState012
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_008 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_47 x xs in
      _menhir_goto_nonempty_list_VAR_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | MODULE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | WHERE ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | IMPORT ->
                      _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState003
                  | RETURN | STYPE | TYPE | VAR _ ->
                      let _v_0 = _menhir_action_43 () in
                      _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState003 _tok
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
end

let main =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_main v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

# 163 "lib/parser.mly"
  

# 5514 "lib/parser.ml"
