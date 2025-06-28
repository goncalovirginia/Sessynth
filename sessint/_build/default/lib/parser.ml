
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | WAIT
    | VAR of (
# 5 "lib/parser.mly"
       (string)
# 16 "lib/parser.ml"
  )
    | UNIT_VAL
    | T_VAR of (
# 6 "lib/parser.mly"
       (string)
# 22 "lib/parser.ml"
  )
    | TYPE
    | TUNIT
    | TNUM
    | THEN
    | TBOOL
    | S_VAR of (
# 7 "lib/parser.mly"
       (string)
# 32 "lib/parser.ml"
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
# 64 "lib/parser.ml"
  )
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
    | BOOL of (
# 9 "lib/parser.mly"
       (bool)
# 86 "lib/parser.ml"
  )
    | AND
    | AMPERSAND
  
end

include MenhirBasics

# 1 "lib/parser.mly"
  
  open InputSyntax

# 99 "lib/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState000 : ('s, _menhir_box_main) _menhir_state
    (** State 000.
        Stack shape : .
        Start symbol: main. *)

  | MenhirState002 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 002.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState007 : (('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_state
    (** State 007.
        Stack shape : L_PAR.
        Start symbol: main. *)

  | MenhirState008 : (('s, _menhir_box_main) _menhir_cell1_L_BRACE, _menhir_box_main) _menhir_state
    (** State 008.
        Stack shape : L_BRACE.
        Start symbol: main. *)

  | MenhirState013 : (('s, _menhir_box_main) _menhir_cell1_REC _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 013.
        Stack shape : REC VAR.
        Start symbol: main. *)

  | MenhirState015 : (('s, _menhir_box_main) _menhir_cell1_PLUS, _menhir_box_main) _menhir_state
    (** State 015.
        Stack shape : PLUS.
        Start symbol: main. *)

  | MenhirState017 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 017.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState018 : (('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_state
    (** State 018.
        Stack shape : L_PAR.
        Start symbol: main. *)

  | MenhirState021 : (('s, _menhir_box_main) _menhir_cell1_AMPERSAND, _menhir_box_main) _menhir_state
    (** State 021.
        Stack shape : AMPERSAND.
        Start symbol: main. *)

  | MenhirState024 : ((('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_state
    (** State 024.
        Stack shape : L_PAR ty.
        Start symbol: main. *)

  | MenhirState026 : ((('s, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_cell1_RIGHT_ARROW_BOLD, _menhir_box_main) _menhir_state
    (** State 026.
        Stack shape : ty RIGHT_ARROW_BOLD.
        Start symbol: main. *)

  | MenhirState027 : (('s, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_state
    (** State 027.
        Stack shape : ty.
        Start symbol: main. *)

  | MenhirState028 : ((('s, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_cell1_CIRCUMFLEX, _menhir_box_main) _menhir_state
    (** State 028.
        Stack shape : ty CIRCUMFLEX.
        Start symbol: main. *)

  | MenhirState032 : (('s, _menhir_box_main) _menhir_cell1_fun_ty_list, _menhir_box_main) _menhir_state
    (** State 032.
        Stack shape : fun_ty_list.
        Start symbol: main. *)

  | MenhirState037 : ((('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_stype, _menhir_box_main) _menhir_state
    (** State 037.
        Stack shape : L_PAR stype.
        Start symbol: main. *)

  | MenhirState039 : ((('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_stype, _menhir_box_main) _menhir_state
    (** State 039.
        Stack shape : L_PAR stype.
        Start symbol: main. *)

  | MenhirState042 : ((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_stype, _menhir_box_main) _menhir_state
    (** State 042.
        Stack shape : VAR stype.
        Start symbol: main. *)

  | MenhirState049 : ((('s, _menhir_box_main) _menhir_cell1_L_BRACE, _menhir_box_main) _menhir_cell1_stype, _menhir_box_main) _menhir_state
    (** State 049.
        Stack shape : L_BRACE stype.
        Start symbol: main. *)

  | MenhirState051 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 051.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState053 : ((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_stype, _menhir_box_main) _menhir_state
    (** State 053.
        Stack shape : VAR stype.
        Start symbol: main. *)

  | MenhirState057 : ((('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_state
    (** State 057.
        Stack shape : L_PAR ty.
        Start symbol: main. *)

  | MenhirState058 : ((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_state
    (** State 058.
        Stack shape : VAR ty.
        Start symbol: main. *)

  | MenhirState061 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 061.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState064 : (('s, _menhir_box_main) _menhir_cell1_WAIT _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 064.
        Stack shape : WAIT VAR.
        Start symbol: main. *)

  | MenhirState068 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 068.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState072 : (('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_state
    (** State 072.
        Stack shape : L_PAR.
        Start symbol: main. *)

  | MenhirState073 : ((('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_state
    (** State 073.
        Stack shape : L_PAR L_PAR.
        Start symbol: main. *)

  | MenhirState076 : (('s, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 076.
        Stack shape : LET VAR.
        Start symbol: main. *)

  | MenhirState078 : (('s, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_state
    (** State 078.
        Stack shape : IF.
        Start symbol: main. *)

  | MenhirState079 : (('s, _menhir_box_main) _menhir_cell1_FUN, _menhir_box_main) _menhir_state
    (** State 079.
        Stack shape : FUN.
        Start symbol: main. *)

  | MenhirState081 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 081.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState083 : (('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_state
    (** State 083.
        Stack shape : L_PAR.
        Start symbol: main. *)

  | MenhirState084 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 084.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState087 : ((('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_nonempty_list_VAR_, _menhir_box_main) _menhir_state
    (** State 087.
        Stack shape : L_PAR nonempty_list(VAR).
        Start symbol: main. *)

  | MenhirState091 : ((('s, _menhir_box_main) _menhir_cell1_FUN, _menhir_box_main) _menhir_cell1_nonempty_list_fun_binder_, _menhir_box_main) _menhir_state
    (** State 091.
        Stack shape : FUN nonempty_list(fun_binder).
        Start symbol: main. *)

  | MenhirState093 : (('s, _menhir_box_main) _menhir_cell1_uop, _menhir_box_main) _menhir_state
    (** State 093.
        Stack shape : uop.
        Start symbol: main. *)

  | MenhirState095 : (('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_state
    (** State 095.
        Stack shape : L_PAR.
        Start symbol: main. *)

  | MenhirState096 : (('s, _menhir_box_main) _menhir_cell1_simple_exp, _menhir_box_main) _menhir_state
    (** State 096.
        Stack shape : simple_exp.
        Start symbol: main. *)

  | MenhirState097 : ((('s, _menhir_box_main) _menhir_cell1_simple_exp, _menhir_box_main) _menhir_cell1_QUESTION, _menhir_box_main) _menhir_state
    (** State 097.
        Stack shape : simple_exp QUESTION.
        Start symbol: main. *)

  | MenhirState099 : (((('s, _menhir_box_main) _menhir_cell1_simple_exp, _menhir_box_main) _menhir_cell1_QUESTION, _menhir_box_main) _menhir_cell1_simple_exp, _menhir_box_main) _menhir_state
    (** State 099.
        Stack shape : simple_exp QUESTION simple_exp.
        Start symbol: main. *)

  | MenhirState101 : ((('s, _menhir_box_main) _menhir_cell1_simple_exp, _menhir_box_main) _menhir_cell1_simple_exp, _menhir_box_main) _menhir_state
    (** State 101.
        Stack shape : simple_exp simple_exp.
        Start symbol: main. *)

  | MenhirState107 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 107.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState109 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 109.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState111 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 111.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState116 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 116.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState118 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 118.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState120 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 120.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState122 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 122.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState124 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 124.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState126 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 126.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState131 : (('s, _menhir_box_main) _menhir_cell1_fun_binder, _menhir_box_main) _menhir_state
    (** State 131.
        Stack shape : fun_binder.
        Start symbol: main. *)

  | MenhirState134 : ((('s, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 134.
        Stack shape : IF expression.
        Start symbol: main. *)

  | MenhirState136 : (((('s, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 136.
        Stack shape : IF expression expression.
        Start symbol: main. *)

  | MenhirState140 : ((('s, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 140.
        Stack shape : LET VAR expression.
        Start symbol: main. *)

  | MenhirState143 : (((('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_state
    (** State 143.
        Stack shape : L_PAR L_PAR ty.
        Start symbol: main. *)

  | MenhirState146 : ((('s, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_state
    (** State 146.
        Stack shape : L_PAR ty.
        Start symbol: main. *)

  | MenhirState148 : ((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 148.
        Stack shape : VAR expression.
        Start symbol: main. *)

  | MenhirState149 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 149.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState152 : (((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_cell1_list_VAR_, _menhir_box_main) _menhir_state
    (** State 152.
        Stack shape : VAR expression list(VAR).
        Start symbol: main. *)

  | MenhirState156 : (('s, _menhir_box_main) _menhir_cell1_SEND_CHAN _menhir_cell0_VAR _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 156.
        Stack shape : SEND_CHAN VAR VAR.
        Start symbol: main. *)

  | MenhirState158 : (('s, _menhir_box_main) _menhir_cell1_SEND _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 158.
        Stack shape : SEND VAR.
        Start symbol: main. *)

  | MenhirState160 : ((('s, _menhir_box_main) _menhir_cell1_SEND _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 160.
        Stack shape : SEND VAR expression.
        Start symbol: main. *)

  | MenhirState161 : (('s, _menhir_box_main) _menhir_cell1_PRINT, _menhir_box_main) _menhir_state
    (** State 161.
        Stack shape : PRINT.
        Start symbol: main. *)

  | MenhirState163 : ((('s, _menhir_box_main) _menhir_cell1_PRINT, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 163.
        Stack shape : PRINT expression.
        Start symbol: main. *)

  | MenhirState164 : (('s, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_state
    (** State 164.
        Stack shape : IF.
        Start symbol: main. *)

  | MenhirState166 : ((('s, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 166.
        Stack shape : IF expression.
        Start symbol: main. *)

  | MenhirState174 : (('s, _menhir_box_main) _menhir_cell1_CASE _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 174.
        Stack shape : CASE VAR.
        Start symbol: main. *)

  | MenhirState177 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 177.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState179 : ((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_proc, _menhir_box_main) _menhir_state
    (** State 179.
        Stack shape : VAR proc.
        Start symbol: main. *)

  | MenhirState183 : (((('s, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_cell1_proc, _menhir_box_main) _menhir_state
    (** State 183.
        Stack shape : IF expression proc.
        Start symbol: main. *)

  | MenhirState189 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 189.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState191 : ((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_proc, _menhir_box_main) _menhir_state
    (** State 191.
        Stack shape : VAR proc.
        Start symbol: main. *)

  | MenhirState193 : (((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_proc, _menhir_box_main) _menhir_cell1_list_VAR_, _menhir_box_main) _menhir_state
    (** State 193.
        Stack shape : VAR proc list(VAR).
        Start symbol: main. *)

  | MenhirState197 : (('s, _menhir_box_main) _menhir_cell1_VAR _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 197.
        Stack shape : VAR VAR.
        Start symbol: main. *)

  | MenhirState201 : (('s, _menhir_box_main) _menhir_cell1_VAR _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 201.
        Stack shape : VAR VAR.
        Start symbol: main. *)

  | MenhirState205 : (('s, _menhir_box_main) _menhir_cell1_VAR _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 205.
        Stack shape : VAR VAR.
        Start symbol: main. *)

  | MenhirState207 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 207.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState208 : ((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_state
    (** State 208.
        Stack shape : VAR ty.
        Start symbol: main. *)

  | MenhirState212 : (((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_cell1_LEFT_ARROW _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 212.
        Stack shape : VAR ty LEFT_ARROW VAR.
        Start symbol: main. *)

  | MenhirState218 : ((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_stype _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 218.
        Stack shape : VAR stype VAR.
        Start symbol: main. *)

  | MenhirState226 : (('s, _menhir_box_main) _menhir_cell1_TYPE _menhir_cell0_T_VAR, _menhir_box_main) _menhir_state
    (** State 226.
        Stack shape : TYPE T_VAR.
        Start symbol: main. *)

  | MenhirState230 : (('s, _menhir_box_main) _menhir_cell1_STYPE _menhir_cell0_S_VAR, _menhir_box_main) _menhir_state
    (** State 230.
        Stack shape : STYPE S_VAR.
        Start symbol: main. *)

  | MenhirState235 : (('s, _menhir_box_main) _menhir_cell1_list_declaration_, _menhir_box_main) _menhir_state
    (** State 235.
        Stack shape : list(declaration).
        Start symbol: main. *)

  | MenhirState240 : (('s, _menhir_box_main) _menhir_cell1_declaration, _menhir_box_main) _menhir_state
    (** State 240.
        Stack shape : declaration.
        Start symbol: main. *)


and ('s, 'r) _menhir_cell1_declaration = 
  | MenhirCell1_declaration of 's * ('s, 'r) _menhir_state * (InputSyntax.decl)

and ('s, 'r) _menhir_cell1_expression = 
  | MenhirCell1_expression of 's * ('s, 'r) _menhir_state * (InputSyntax.exp)

and ('s, 'r) _menhir_cell1_fun_binder = 
  | MenhirCell1_fun_binder of 's * ('s, 'r) _menhir_state * (string list * InputSyntax.ty option)

and ('s, 'r) _menhir_cell1_fun_ty_list = 
  | MenhirCell1_fun_ty_list of 's * ('s, 'r) _menhir_state * (InputSyntax.ty list)

and ('s, 'r) _menhir_cell1_list_VAR_ = 
  | MenhirCell1_list_VAR_ of 's * ('s, 'r) _menhir_state * (string list)

and ('s, 'r) _menhir_cell1_list_declaration_ = 
  | MenhirCell1_list_declaration_ of 's * ('s, 'r) _menhir_state * (InputSyntax.decl list)

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
# 617 "lib/parser.ml"
)

and ('s, 'r) _menhir_cell1_TYPE = 
  | MenhirCell1_TYPE of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_T_VAR = 
  | MenhirCell0_T_VAR of 's * (
# 6 "lib/parser.mly"
       (string)
# 627 "lib/parser.ml"
)

and ('s, 'r) _menhir_cell1_VAR = 
  | MenhirCell1_VAR of 's * ('s, 'r) _menhir_state * (
# 5 "lib/parser.mly"
       (string)
# 634 "lib/parser.ml"
)

and 's _menhir_cell0_VAR = 
  | MenhirCell0_VAR of 's * (
# 5 "lib/parser.mly"
       (string)
# 641 "lib/parser.ml"
)

and ('s, 'r) _menhir_cell1_WAIT = 
  | MenhirCell1_WAIT of 's * ('s, 'r) _menhir_state

and _menhir_box_main = 
  | MenhirBox_main of (InputSyntax.prog) [@@unboxed]

let _menhir_action_01 =
  fun e t ->
    (
# 100 "lib/parser.mly"
                                      ( Annot (e, t) )
# 655 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_02 =
  fun e1 e2 ->
    (
# 103 "lib/parser.mly"
                                      ( FunApp (e1, e2) )
# 663 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_03 =
  fun l p ->
    (
# 92 "lib/parser.mly"
                                                       ( [(l, p)] )
# 671 "lib/parser.ml"
     : ((string * InputSyntax.proc) list))

let _menhir_action_04 =
  fun cl l p ->
    (
# 93 "lib/parser.mly"
                                                       ( (l, p)::cl )
# 679 "lib/parser.ml"
     : ((string * InputSyntax.proc) list))

let _menhir_action_05 =
  fun st v ->
    (
# 88 "lib/parser.mly"
                                                    ( [(v, st)] )
# 687 "lib/parser.ml"
     : ((string * InputSyntax.stype) list))

let _menhir_action_06 =
  fun cl st v ->
    (
# 89 "lib/parser.mly"
                                                    ( (v,st)::cl )
# 695 "lib/parser.ml"
     : ((string * InputSyntax.stype) list))

let _menhir_action_07 =
  fun cond e1 e2 ->
    (
# 96 "lib/parser.mly"
                                                                         ( Cond (cond, e1, e2) )
# 703 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_08 =
  fun cond e1 e2 ->
    (
# 97 "lib/parser.mly"
                                                                         ( Cond (cond, e1, e2) )
# 711 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_09 =
  fun st v ->
    (
# 37 "lib/parser.mly"
                                                      ( Decl (v, Annot (Var v, TProc(st, []))) )
# 719 "lib/parser.ml"
     : (InputSyntax.decl))

let _menhir_action_10 =
  fun t v ->
    (
# 38 "lib/parser.mly"
                                                      ( Decl (v, Annot (Var v, t)))
# 727 "lib/parser.ml"
     : (InputSyntax.decl))

let _menhir_action_11 =
  fun e t var ->
    (
# 39 "lib/parser.mly"
                                                      ( Decl (var, Annot (e, t)) )
# 735 "lib/parser.ml"
     : (InputSyntax.decl))

let _menhir_action_12 =
  fun e ->
    (
# 53 "lib/parser.mly"
                   ( ExecExp e )
# 743 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_13 =
  fun e ->
    (
# 54 "lib/parser.mly"
                   ( ExecExp e )
# 751 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_14 =
  fun _1 ->
    (
# 42 "lib/parser.mly"
                                                                      ( _1 )
# 759 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_15 =
  fun body l ->
    (
# 43 "lib/parser.mly"
                                                                      ( FunDef (l, body) )
# 767 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_16 =
  fun e1 e2 var ->
    (
# 44 "lib/parser.mly"
                                                                      ( Let (var, e1, e2) )
# 775 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_17 =
  fun e1 e2 ->
    let op = 
# 143 "lib/parser.mly"
              ( Mul )
# 783 "lib/parser.ml"
     in
    (
# 45 "lib/parser.mly"
                                                                      ( BOp (op, e1, e2) )
# 788 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_18 =
  fun e1 e2 ->
    let op = 
# 144 "lib/parser.mly"
              ( Div )
# 796 "lib/parser.ml"
     in
    (
# 45 "lib/parser.mly"
                                                                      ( BOp (op, e1, e2) )
# 801 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_19 =
  fun e1 e2 ->
    let op = 
# 145 "lib/parser.mly"
              ( And )
# 809 "lib/parser.ml"
     in
    (
# 45 "lib/parser.mly"
                                                                      ( BOp (op, e1, e2) )
# 814 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_20 =
  fun e1 e2 ->
    let op = 
# 146 "lib/parser.mly"
              ( Or )
# 822 "lib/parser.ml"
     in
    (
# 45 "lib/parser.mly"
                                                                      ( BOp (op, e1, e2) )
# 827 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_21 =
  fun e1 e2 ->
    let op = 
# 147 "lib/parser.mly"
              ( Add )
# 835 "lib/parser.ml"
     in
    (
# 45 "lib/parser.mly"
                                                                      ( BOp (op, e1, e2) )
# 840 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_22 =
  fun e1 e2 ->
    let op = 
# 148 "lib/parser.mly"
              ( Sub )
# 848 "lib/parser.ml"
     in
    (
# 45 "lib/parser.mly"
                                                                      ( BOp (op, e1, e2) )
# 853 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_23 =
  fun e1 e2 ->
    let op = 
# 149 "lib/parser.mly"
              ( Lesser )
# 861 "lib/parser.ml"
     in
    (
# 45 "lib/parser.mly"
                                                                      ( BOp (op, e1, e2) )
# 866 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_24 =
  fun e1 e2 ->
    let op = 
# 150 "lib/parser.mly"
              ( Greater )
# 874 "lib/parser.ml"
     in
    (
# 45 "lib/parser.mly"
                                                                      ( BOp (op, e1, e2) )
# 879 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_25 =
  fun e1 e2 ->
    let op = 
# 151 "lib/parser.mly"
              ( Equals )
# 887 "lib/parser.ml"
     in
    (
# 45 "lib/parser.mly"
                                                                      ( BOp (op, e1, e2) )
# 892 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_26 =
  fun e op ->
    (
# 46 "lib/parser.mly"
                                                                      ( UOp (op, e) )
# 900 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_27 =
  fun _1 ->
    (
# 47 "lib/parser.mly"
                                                                      ( _1 )
# 908 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_28 =
  fun _1 ->
    (
# 48 "lib/parser.mly"
                                                                      ( _1 )
# 916 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_29 =
  fun _1 ->
    (
# 49 "lib/parser.mly"
                                                                      ( _1 )
# 924 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_30 =
  fun _1 ->
    (
# 50 "lib/parser.mly"
                                                                      ( _1 )
# 932 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_31 =
  fun x ->
    (
# 113 "lib/parser.mly"
                                              ( ([x], None) )
# 940 "lib/parser.ml"
     : (string list * InputSyntax.ty option))

let _menhir_action_32 =
  fun t x ->
    (
# 114 "lib/parser.mly"
                                              ( ([x], Some t) )
# 948 "lib/parser.ml"
     : (string list * InputSyntax.ty option))

let _menhir_action_33 =
  fun lx t ->
    (
# 115 "lib/parser.mly"
                                              ( (lx, Some t) )
# 956 "lib/parser.ml"
     : (string list * InputSyntax.ty option))

let _menhir_action_34 =
  fun _1 ->
    (
# 135 "lib/parser.mly"
                                              ( [_1] )
# 964 "lib/parser.ml"
     : (InputSyntax.ty list))

let _menhir_action_35 =
  fun l t ->
    (
# 136 "lib/parser.mly"
                                              ( l@[t] )
# 972 "lib/parser.ml"
     : (InputSyntax.ty list))

let _menhir_action_36 =
  fun st v ->
    (
# 131 "lib/parser.mly"
                                                 ( [(v, st)] )
# 980 "lib/parser.ml"
     : ((string * InputSyntax.stype) list))

let _menhir_action_37 =
  fun cl st v ->
    (
# 132 "lib/parser.mly"
                                                 ( (v,st)::cl )
# 988 "lib/parser.ml"
     : ((string * InputSyntax.stype) list))

let _menhir_action_38 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 996 "lib/parser.ml"
     : (string list))

let _menhir_action_39 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 1004 "lib/parser.ml"
     : (string list))

let _menhir_action_40 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 1012 "lib/parser.ml"
     : (InputSyntax.decl list))

let _menhir_action_41 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 1020 "lib/parser.ml"
     : (InputSyntax.decl list))

let _menhir_action_42 =
  fun d e ->
    (
# 34 "lib/parser.mly"
                                                ( Prog (d, e) )
# 1028 "lib/parser.ml"
     : (InputSyntax.prog))

let _menhir_action_43 =
  fun x ->
    (
# 228 "<standard.mly>"
    ( [ x ] )
# 1036 "lib/parser.ml"
     : (string list))

let _menhir_action_44 =
  fun x xs ->
    (
# 231 "<standard.mly>"
    ( x :: xs )
# 1044 "lib/parser.ml"
     : (string list))

let _menhir_action_45 =
  fun x ->
    (
# 228 "<standard.mly>"
    ( [ x ] )
# 1052 "lib/parser.ml"
     : ((string list * InputSyntax.ty option) list))

let _menhir_action_46 =
  fun x xs ->
    (
# 231 "<standard.mly>"
    ( x :: xs )
# 1060 "lib/parser.ml"
     : ((string list * InputSyntax.ty option) list))

let _menhir_action_47 =
  fun x ->
    (
# 228 "<standard.mly>"
    ( [ x ] )
# 1068 "lib/parser.ml"
     : (InputSyntax.exp list))

let _menhir_action_48 =
  fun x xs ->
    (
# 231 "<standard.mly>"
    ( x :: xs )
# 1076 "lib/parser.ml"
     : (InputSyntax.exp list))

let _menhir_action_49 =
  fun c e p ->
    (
# 60 "lib/parser.mly"
                                                                                        ( Send (c, e, p) )
# 1084 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_50 =
  fun c id p t ->
    (
# 61 "lib/parser.mly"
                                                                                        ( Recv (id, c, Some t, p) )
# 1092 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_51 =
  fun c id p ->
    (
# 62 "lib/parser.mly"
                                                                                        ( Recv (id, c, None, p) )
# 1100 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_52 =
  fun c ->
    (
# 63 "lib/parser.mly"
                                                                                        ( Close (c) )
# 1108 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_53 =
  fun c p ->
    (
# 64 "lib/parser.mly"
                                                                                        ( Wait (c, p) )
# 1116 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_54 =
  fun c1 c2 ->
    (
# 65 "lib/parser.mly"
                                                                                        ( Fwd (c1, c2) )
# 1124 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_55 =
  fun args c e p ->
    (
# 66 "lib/parser.mly"
                                                                                        ( Spawn (c, e, p, args) )
# 1132 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_56 =
  fun args c p1 p2 ->
    (
# 67 "lib/parser.mly"
                                                                                        ( Spawn (c, ProcExp(c, p1), p2, args) )
# 1140 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_57 =
  fun c l ->
    (
# 68 "lib/parser.mly"
                                                                                        ( Choice (c, l) )
# 1148 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_58 =
  fun c l p ->
    (
# 69 "lib/parser.mly"
                                                                                        ( Label (c, l, p) )
# 1156 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_59 =
  fun c e p ->
    (
# 70 "lib/parser.mly"
                                                                                        ( SendChan (c, e, p) )
# 1164 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_60 =
  fun c id p st ->
    (
# 71 "lib/parser.mly"
                                                                                        ( RecvChan (id, c, Some st, p) )
# 1172 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_61 =
  fun c id p ->
    (
# 72 "lib/parser.mly"
                                                                                        ( RecvChan (id, c, None, p) )
# 1180 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_62 =
  fun e p ->
    (
# 73 "lib/parser.mly"
                                                                                        ( Print (e, p) )
# 1188 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_63 =
  fun e p1 p2 ->
    (
# 74 "lib/parser.mly"
                                                                                        ( If (e, p1, p2) )
# 1196 "lib/parser.ml"
     : (InputSyntax.proc))

let _menhir_action_64 =
  fun c p ->
    (
# 57 "lib/parser.mly"
                                                ( ProcExp (c, p) )
# 1204 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_65 =
  fun () ->
    (
# 106 "lib/parser.mly"
                                    ( UnitVal )
# 1212 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_66 =
  fun i ->
    (
# 107 "lib/parser.mly"
                                    ( Num (i) )
# 1220 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_67 =
  fun b ->
    (
# 108 "lib/parser.mly"
                                    ( Bool (b) )
# 1228 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_68 =
  fun v ->
    (
# 109 "lib/parser.mly"
                                    ( Var (v) )
# 1236 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_69 =
  fun e ->
    (
# 110 "lib/parser.mly"
                                    ( e )
# 1244 "lib/parser.ml"
     : (InputSyntax.exp))

let _menhir_action_70 =
  fun () ->
    (
# 122 "lib/parser.mly"
                                                          ( TUnit )
# 1252 "lib/parser.ml"
     : (InputSyntax.ty))

let _menhir_action_71 =
  fun () ->
    (
# 123 "lib/parser.mly"
                                                          ( TNum )
# 1260 "lib/parser.ml"
     : (InputSyntax.ty))

let _menhir_action_72 =
  fun () ->
    (
# 124 "lib/parser.mly"
                                                          ( TBool )
# 1268 "lib/parser.ml"
     : (InputSyntax.ty))

let _menhir_action_73 =
  fun s ->
    (
# 125 "lib/parser.mly"
                                                          ( TProc (s, []))
# 1276 "lib/parser.ml"
     : (InputSyntax.ty))

let _menhir_action_74 =
  fun ctxt s ->
    (
# 126 "lib/parser.mly"
                                                          ( TProc (s, ctxt))
# 1284 "lib/parser.ml"
     : (InputSyntax.ty))

let _menhir_action_75 =
  fun t ->
    (
# 127 "lib/parser.mly"
                                                          ( t )
# 1292 "lib/parser.ml"
     : (InputSyntax.ty))

let _menhir_action_76 =
  fun x ->
    (
# 128 "lib/parser.mly"
                                                          ( TVar x )
# 1300 "lib/parser.ml"
     : (InputSyntax.ty))

let _menhir_action_77 =
  fun v ->
    (
# 76 "lib/parser.mly"
                                                 ( STVar (v) )
# 1308 "lib/parser.ml"
     : (InputSyntax.stype))

let _menhir_action_78 =
  fun sty v ->
    (
# 77 "lib/parser.mly"
                                                 ( STRec (v, sty) )
# 1316 "lib/parser.ml"
     : (InputSyntax.stype))

let _menhir_action_79 =
  fun st t ->
    (
# 78 "lib/parser.mly"
                                                 ( STSend (t, st) )
# 1324 "lib/parser.ml"
     : (InputSyntax.stype))

let _menhir_action_80 =
  fun st t ->
    (
# 79 "lib/parser.mly"
                                                 ( STRecv (t, st) )
# 1332 "lib/parser.ml"
     : (InputSyntax.stype))

let _menhir_action_81 =
  fun () ->
    (
# 80 "lib/parser.mly"
                                                 ( STEnd )
# 1340 "lib/parser.ml"
     : (InputSyntax.stype))

let _menhir_action_82 =
  fun l ->
    (
# 81 "lib/parser.mly"
                                                 ( STExtChoice (l) )
# 1348 "lib/parser.ml"
     : (InputSyntax.stype))

let _menhir_action_83 =
  fun l ->
    (
# 82 "lib/parser.mly"
                                                 ( STIntChoice (l) )
# 1356 "lib/parser.ml"
     : (InputSyntax.stype))

let _menhir_action_84 =
  fun st1 st2 ->
    (
# 83 "lib/parser.mly"
                                                 ( STSendChan (st1, st2) )
# 1364 "lib/parser.ml"
     : (InputSyntax.stype))

let _menhir_action_85 =
  fun st1 st2 ->
    (
# 84 "lib/parser.mly"
                                                 ( STRecvChan (st1, st2) )
# 1372 "lib/parser.ml"
     : (InputSyntax.stype))

let _menhir_action_86 =
  fun v ->
    (
# 85 "lib/parser.mly"
                                                 ( STUVar (v) )
# 1380 "lib/parser.ml"
     : (InputSyntax.stype))

let _menhir_action_87 =
  fun _1 ->
    (
# 118 "lib/parser.mly"
                                               ( _1 )
# 1388 "lib/parser.ml"
     : (InputSyntax.ty))

let _menhir_action_88 =
  fun l t ->
    (
# 119 "lib/parser.mly"
                                               ( TFun(l, t) )
# 1396 "lib/parser.ml"
     : (InputSyntax.ty))

let _menhir_action_89 =
  fun () ->
    (
# 139 "lib/parser.mly"
          ( Neg )
# 1404 "lib/parser.ml"
     : (InputSyntax.uop))

let _menhir_action_90 =
  fun () ->
    (
# 140 "lib/parser.mly"
          ( Not )
# 1412 "lib/parser.ml"
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

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_goto_exec_exp : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_list_declaration_ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RETURN ->
          let MenhirCell1_list_declaration_ (_menhir_stack, _, d) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_42 d e in
          MenhirBox_main _v
      | _ ->
          _eRR ()
  
  let _menhir_run_236 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_list_declaration_ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _v _tok ->
      let e = _v in
      let _v = _menhir_action_13 e in
      _menhir_goto_exec_exp _menhir_stack _v _tok
  
  let _menhir_run_237 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_list_declaration_ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _v _tok ->
      let e = _v in
      let _v = _menhir_action_12 e in
      _menhir_goto_exec_exp _menhir_stack _v _tok
  
  let rec _menhir_run_001 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _menhir_s = MenhirState002 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | T_VAR _v ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TUNIT ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TNUM ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TBOOL ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_BRACE ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_003 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let x = _v in
      let _v = _menhir_action_76 x in
      _menhir_goto_simple_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_simple_ty : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState087 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState081 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState032 ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState230 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState226 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState002 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState207 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState007 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState008 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState013 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState017 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState018 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState039 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState026 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState028 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_088 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_nonempty_list_VAR_ -> _ -> _ -> _ -> _ -> _menhir_box_main =
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
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState131
      | L_PAR ->
          let _menhir_stack = MenhirCell1_fun_binder (_menhir_stack, _menhir_s, _v) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | RIGHT_ARROW ->
          let x = _v in
          let _v = _menhir_action_45 x in
          _menhir_goto_nonempty_list_fun_binder_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_080 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState081 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | T_VAR _v ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TUNIT ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TNUM ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TBOOL ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_BRACE ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | L_PAR | RIGHT_ARROW | VAR _ ->
          let x = _v in
          let _v = _menhir_action_31 x in
          _menhir_goto_fun_binder _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_004 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_70 () in
      _menhir_goto_simple_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_005 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_71 () in
      _menhir_goto_simple_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_006 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_72 () in
      _menhir_goto_simple_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_007 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_L_PAR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState007 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_VAR _v ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TUNIT ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TNUM ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TBOOL ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_BRACE ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_008 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_L_BRACE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState008 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | T_VAR _v ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TUNIT ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TNUM ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TBOOL ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | S_VAR _v ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REC ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_BRACE ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | END_STYPE ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMPERSAND ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_009 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let v = _v in
      let _v = _menhir_action_77 v in
      _menhir_goto_stype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_stype : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState230 ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState207 ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState008 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState013 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState017 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState039 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState037 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState018 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState026 ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState028 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_231 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_STYPE _menhir_cell0_S_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
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
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState240
      | TYPE ->
          _menhir_run_225 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState240
      | STYPE ->
          _menhir_run_229 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState240
      | RETURN ->
          let _v_1 = _menhir_action_40 () in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | _ ->
          _eRR ()
  
  and _menhir_run_225 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TYPE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_VAR _v ->
          let _menhir_stack = MenhirCell0_T_VAR (_menhir_stack, _v) in
          let _menhir_s = MenhirState226 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | T_VAR _v ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TUNIT ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TNUM ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TBOOL ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_BRACE ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_229 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_STYPE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | S_VAR _v ->
          let _menhir_stack = MenhirCell0_S_VAR (_menhir_stack, _v) in
          let _menhir_s = MenhirState230 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | T_VAR _v ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TUNIT ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TNUM ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TBOOL ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | S_VAR _v ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REC ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_BRACE ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | END_STYPE ->
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMPERSAND ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_010 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let v = _v in
      let _v = _menhir_action_86 v in
      _menhir_goto_stype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_011 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_REC (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | DOT ->
              let _menhir_s = MenhirState013 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | T_VAR _v ->
                  _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TUNIT ->
                  _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | TNUM ->
                  _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | TBOOL ->
                  _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | S_VAR _v ->
                  _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | REC ->
                  _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PLUS ->
                  _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | L_PAR ->
                  _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | L_BRACE ->
                  _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | END_STYPE ->
                  _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | AMPERSAND ->
                  _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_014 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PLUS (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | L_BRACE ->
          let _menhir_s = MenhirState015 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_016 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _menhir_s = MenhirState017 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | T_VAR _v ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TUNIT ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TNUM ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TBOOL ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | S_VAR _v ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REC ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_BRACE ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | END_STYPE ->
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMPERSAND ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_018 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_L_PAR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState018 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | T_VAR _v ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TUNIT ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TNUM ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TBOOL ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | S_VAR _v ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REC ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_BRACE ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | END_STYPE ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMPERSAND ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_019 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_81 () in
      _menhir_goto_stype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_020 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_AMPERSAND (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | L_BRACE ->
          let _menhir_s = MenhirState021 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_241 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_declaration -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_declaration (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_41 x xs in
      _menhir_goto_list_declaration_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_list_declaration_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState240 ->
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState000 ->
          _menhir_run_234 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_234 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_list_declaration_ (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState235 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_059 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LEFT_ARROW ->
          let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | L_BRACE ->
              let _menhir_s = MenhirState061 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | WAIT ->
                  _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | VAR _v ->
                  _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | SEND_CHAN ->
                  _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | SEND ->
                  _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PRINT ->
                  _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IF ->
                  _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FWD ->
                  _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CLOSE ->
                  _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CASE ->
                  _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | AND | BOOL _ | DIV | ELSE | END | ENDIF | EQUALS | GREATER | IN | INT _ | LESSER | L_PAR | MINUS | MULT | OR | PLUS | QUESTION | RETURN | R_PAR | SEMI_COLON | THEN | UNIT_VAL | VAR _ ->
          let v = _v in
          let _v = _menhir_action_68 v in
          _menhir_goto_simple_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_062 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_WAIT (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SEMI_COLON ->
              let _menhir_s = MenhirState064 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | WAIT ->
                  _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | VAR _v ->
                  _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | SEND_CHAN ->
                  _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | SEND ->
                  _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PRINT ->
                  _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IF ->
                  _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FWD ->
                  _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CLOSE ->
                  _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CASE ->
                  _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_065 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
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
                  let _menhir_s = MenhirState068 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | VAR _v ->
                      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | UNIT_VAL ->
                      _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | NOT ->
                      _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | MINUS ->
                      _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | L_PAR ->
                      _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LET ->
                      _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | INT _v ->
                      _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | IF ->
                      _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | FUN ->
                      _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | BOOL _v ->
                      _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | _ ->
                      _eRR ())
              | L_BRACE ->
                  let _menhir_s = MenhirState189 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | WAIT ->
                      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | VAR _v ->
                      _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | SEND_CHAN ->
                      _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | SEND ->
                      _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | PRINT ->
                      _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | IF ->
                      _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | FWD ->
                      _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | CLOSE ->
                      _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | CASE ->
                      _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
                      let _menhir_s = MenhirState197 in
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      (match (_tok : MenhirBasics.token) with
                      | WAIT ->
                          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | VAR _v ->
                          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                      | SEND_CHAN ->
                          _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | SEND ->
                          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | PRINT ->
                          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | IF ->
                          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | FWD ->
                          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | CLOSE ->
                          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | CASE ->
                          _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
                      let _menhir_s = MenhirState201 in
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      (match (_tok : MenhirBasics.token) with
                      | WAIT ->
                          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | VAR _v ->
                          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                      | SEND_CHAN ->
                          _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | SEND ->
                          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | PRINT ->
                          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | IF ->
                          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | FWD ->
                          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | CLOSE ->
                          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | CASE ->
                          _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
                  let _menhir_s = MenhirState205 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | WAIT ->
                      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | VAR _v ->
                      _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | SEND_CHAN ->
                      _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | SEND ->
                      _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | PRINT ->
                      _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | IF ->
                      _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | FWD ->
                      _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | CLOSE ->
                      _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | CASE ->
                      _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | COLON ->
          let _menhir_s = MenhirState207 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | T_VAR _v ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TUNIT ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TNUM ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TBOOL ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | S_VAR _v ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REC ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_BRACE ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | END_STYPE ->
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMPERSAND ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_069 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_65 () in
      _menhir_goto_simple_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_simple_exp : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState235 ->
          _menhir_run_236 _menhir_stack _v _tok
      | MenhirState093 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState101 ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState099 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState097 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState161 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState068 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState076 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState134 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState118 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState111 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState109 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState107 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_128 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_uop -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_uop (_menhir_stack, _menhir_s, op) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_26 e op in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState058 ->
          _menhir_run_223 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_165 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState161 ->
          _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState068 ->
          _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState076 ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState134 ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_127 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState118 ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState116 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState111 ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState109 ->
          _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState107 ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_223 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_ty as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
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
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_107 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState107 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_070 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_90 () in
      _menhir_goto_uop _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_uop : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_uop (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState093
      | UNIT_VAL ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | L_PAR ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | INT _v_1 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState093
      | BOOL _v_2 ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState093
      | _ ->
          _eRR ()
  
  and _menhir_run_094 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let v = _v in
      let _v = _menhir_action_68 v in
      _menhir_goto_simple_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_095 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_L_PAR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState095 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_071 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_89 () in
      _menhir_goto_uop _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_072 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_L_PAR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState072 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | T_VAR _v ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TUNIT ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TNUM ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TBOOL ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_BRACE ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_073 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR as 'stack) -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_L_PAR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState073 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | T_VAR _v ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TUNIT ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TNUM ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TBOOL ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_BRACE ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_074 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | EQUALS ->
              let _menhir_s = MenhirState076 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | UNIT_VAL ->
                  _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | NOT ->
                  _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MINUS ->
                  _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | L_PAR ->
                  _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LET ->
                  _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT _v ->
                  _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IF ->
                  _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FUN ->
                  _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL _v ->
                  _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_077 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let i = _v in
      let _v = _menhir_action_66 i in
      _menhir_goto_simple_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_078 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState078 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_079 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FUN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState079 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | L_PAR ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_083 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_L_PAR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState083 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_084 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState084
      | COLON ->
          let x = _v in
          let _v = _menhir_action_43 x in
          _menhir_goto_nonempty_list_VAR_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_nonempty_list_VAR_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState083 ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState084 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_086 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_nonempty_list_VAR_ (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState087 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_VAR _v ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TUNIT ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TNUM ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TBOOL ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_BRACE ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_085 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_VAR -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_44 x xs in
      _menhir_goto_nonempty_list_VAR_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_092 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let b = _v in
      let _v = _menhir_action_67 b in
      _menhir_goto_simple_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_109 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState109 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_111 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState111 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_124 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState124 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_116 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState116 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_120 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState120 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_126 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState126 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_118 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState118 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_122 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState122 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_165 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | THEN ->
          let _menhir_s = MenhirState166 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WAIT ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SEND_CHAN ->
              _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEND ->
              _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FWD ->
              _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CLOSE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | PLUS ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_153 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
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
                  let _menhir_s = MenhirState156 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | WAIT ->
                      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | VAR _v ->
                      _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | SEND_CHAN ->
                      _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | SEND ->
                      _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | PRINT ->
                      _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | IF ->
                      _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | FWD ->
                      _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | CLOSE ->
                      _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | CASE ->
                      _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_157 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_SEND (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
          let _menhir_s = MenhirState158 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT_VAL ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL _v ->
              _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_161 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PRINT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState161 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_164 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState164 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_167 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let (c2, c1) = (_v_0, _v) in
              let _v = _menhir_action_54 c1 c2 in
              _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_proc : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState061 ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState064 ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState218 ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState212 ->
          _menhir_run_213 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState205 ->
          _menhir_run_206 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState201 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState197 ->
          _menhir_run_198 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState193 ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState189 ->
          _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState152 ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState156 ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState160 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState163 ->
          _menhir_run_185 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState183 ->
          _menhir_run_184 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState166 ->
          _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState177 ->
          _menhir_run_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_221 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | R_BRACE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_VAR (_menhir_stack, _menhir_s, c) = _menhir_stack in
          let p = _v in
          let _v = _menhir_action_64 c p in
          _menhir_goto_proc_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_proc_exp : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState235 ->
          _menhir_run_237 _menhir_stack _v _tok
      | MenhirState058 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState161 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState068 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState076 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState134 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState118 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState111 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState109 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState107 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_104 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_30 _1 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_220 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_WAIT _menhir_cell0_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_VAR (_menhir_stack, c) = _menhir_stack in
      let MenhirCell1_WAIT (_menhir_stack, _menhir_s) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_53 c p in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_219 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_stype _menhir_cell0_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_VAR (_menhir_stack, c) = _menhir_stack in
      let MenhirCell1_stype (_menhir_stack, _, st) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, id) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_60 c id p st in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_213 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_cell1_LEFT_ARROW _menhir_cell0_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_VAR (_menhir_stack, c) = _menhir_stack in
      let MenhirCell1_LEFT_ARROW (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_ty (_menhir_stack, _, t) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, id) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_50 c id p t in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_206 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_VAR _menhir_cell0_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_VAR (_menhir_stack, l) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, c) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_58 c l p in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_202 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_VAR _menhir_cell0_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_VAR (_menhir_stack, c) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, id) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_51 c id p in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_198 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_VAR _menhir_cell0_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_VAR (_menhir_stack, c) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, id) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_61 c id p in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_194 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_proc, _menhir_box_main) _menhir_cell1_list_VAR_ -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_list_VAR_ (_menhir_stack, _, args) = _menhir_stack in
      let MenhirCell1_proc (_menhir_stack, _, p1) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, c) = _menhir_stack in
      let p2 = _v in
      let _v = _menhir_action_56 args c p1 p2 in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_190 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_proc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | R_BRACE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v_0 ->
              _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState191
          | SEMI_COLON ->
              let _v_1 = _menhir_action_38 () in
              _menhir_run_192 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState191
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_149 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState149
      | SEMI_COLON ->
          let _v_1 = _menhir_action_38 () in
          _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | _ ->
          _eRR ()
  
  and _menhir_run_150 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_VAR -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_39 x xs in
      _menhir_goto_list_VAR_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_list_VAR_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState191 ->
          _menhir_run_192 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState148 ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState149 ->
          _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_192 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_proc as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_list_VAR_ (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState193 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WAIT ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VAR _v ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SEND_CHAN ->
          _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SEND ->
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FWD ->
          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CLOSE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CASE ->
          _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_170 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v in
          let _v = _menhir_action_52 c in
          _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_172 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_CASE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | OF ->
              let _menhir_s = MenhirState174 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_175 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | L_PAR ->
              let _menhir_s = MenhirState177 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | WAIT ->
                  _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | VAR _v ->
                  _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | SEND_CHAN ->
                  _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | SEND ->
                  _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PRINT ->
                  _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IF ->
                  _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FWD ->
                  _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CLOSE ->
                  _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CASE ->
                  _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_151 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_list_VAR_ (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState152 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WAIT ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VAR _v ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SEND_CHAN ->
          _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SEND ->
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FWD ->
          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CLOSE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CASE ->
          _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_188 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_cell1_list_VAR_ -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_list_VAR_ (_menhir_stack, _, args) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, e) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, c) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_55 args c e p in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_187 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_SEND_CHAN _menhir_cell0_VAR _menhir_cell0_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_VAR (_menhir_stack, e) = _menhir_stack in
      let MenhirCell0_VAR (_menhir_stack, c) = _menhir_stack in
      let MenhirCell1_SEND_CHAN (_menhir_stack, _menhir_s) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_59 c e p in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_186 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_SEND _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expression (_menhir_stack, _, e) = _menhir_stack in
      let MenhirCell0_VAR (_menhir_stack, c) = _menhir_stack in
      let MenhirCell1_SEND (_menhir_stack, _menhir_s) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_49 c e p in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_185 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_PRINT, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expression (_menhir_stack, _, e) = _menhir_stack in
      let MenhirCell1_PRINT (_menhir_stack, _menhir_s) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_62 e p in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_184 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_cell1_proc -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_proc (_menhir_stack, _, p1) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, e) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
      let p2 = _v in
      let _v = _menhir_action_63 e p1 p2 in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_182 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_proc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ELSE ->
          let _menhir_s = MenhirState183 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WAIT ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SEND_CHAN ->
              _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEND ->
              _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FWD ->
              _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CLOSE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_178 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | R_PAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v_0 ->
              let _menhir_stack = MenhirCell1_proc (_menhir_stack, _menhir_s, _v) in
              _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState179
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
      | MenhirState174 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState179 ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_181 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_CASE _menhir_cell0_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_VAR (_menhir_stack, c) = _menhir_stack in
      let MenhirCell1_CASE (_menhir_stack, _menhir_s) = _menhir_stack in
      let l = _v in
      let _v = _menhir_action_57 c l in
      _menhir_goto_proc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_180 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_proc -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_proc (_menhir_stack, _, p) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, l) = _menhir_stack in
      let cl = _v in
      let _v = _menhir_action_04 cl l p in
      _menhir_goto_case_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_162 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_PRINT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMI_COLON ->
          let _menhir_s = MenhirState163 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WAIT ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SEND_CHAN ->
              _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEND ->
              _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FWD ->
              _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CLOSE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | PLUS ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_159 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_SEND _menhir_cell0_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMI_COLON ->
          let _menhir_s = MenhirState160 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WAIT ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SEND_CHAN ->
              _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEND ->
              _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FWD ->
              _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CLOSE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | PLUS ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_147 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | R_PAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v_0 ->
              _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState148
          | SEMI_COLON ->
              let _v_1 = _menhir_action_38 () in
              _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState148
          | _ ->
              _eRR ())
      | PLUS ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_144 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_ty as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
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
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_141 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
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
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_139 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IN ->
          let _menhir_s = MenhirState140 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT_VAL ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL _v ->
              _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | GREATER ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_137 : type  ttv_stack. ((((ttv_stack, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
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
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_goto_cond_exp : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_29 _1 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_135 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE ->
          let _menhir_s = MenhirState136 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT_VAL ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL _v ->
              _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | DIV ->
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_133 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | THEN ->
          let _menhir_s = MenhirState134 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT_VAL ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL _v ->
              _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | PLUS ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_129 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_FUN, _menhir_box_main) _menhir_cell1_nonempty_list_fun_binder_ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | END ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_nonempty_list_fun_binder_ (_menhir_stack, _, l) = _menhir_stack in
          let MenhirCell1_FUN (_menhir_stack, _menhir_s) = _menhir_stack in
          let body = _v in
          let _v = _menhir_action_15 body l in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_127 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE | END | ENDIF | EQUALS | IN | R_PAR | SEMI_COLON | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_25 e1 e2 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_125 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE | END | ENDIF | EQUALS | IN | MINUS | PLUS | R_PAR | SEMI_COLON | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_22 e1 e2 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_123 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ELSE | END | ENDIF | EQUALS | IN | MINUS | OR | PLUS | R_PAR | SEMI_COLON | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_19 e1 e2 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_121 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ELSE | END | ENDIF | EQUALS | GREATER | IN | LESSER | MINUS | OR | PLUS | R_PAR | SEMI_COLON | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_24 e1 e2 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_119 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_18 e1 e2 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_117 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ELSE | END | ENDIF | EQUALS | GREATER | IN | LESSER | MINUS | OR | PLUS | R_PAR | SEMI_COLON | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_23 e1 e2 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_112 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_17 e1 e2 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_110 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ELSE | END | ENDIF | EQUALS | IN | MINUS | OR | PLUS | R_PAR | SEMI_COLON | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_20 e1 e2 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_108 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE | END | ENDIF | EQUALS | IN | MINUS | PLUS | R_PAR | SEMI_COLON | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_21 e1 e2 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_105 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | R_PAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_L_PAR (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_69 e in
          _menhir_goto_simple_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LESSER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATER ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_101 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_simple_exp as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState101
      | UNIT_VAL ->
          let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | L_PAR ->
          let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | INT _v_1 ->
          let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState101
      | BOOL _v_2 ->
          let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState101
      | AND | DIV | ELSE | END | ENDIF | EQUALS | GREATER | IN | LESSER | MINUS | MULT | OR | PLUS | R_PAR | SEMI_COLON | THEN ->
          let x = _v in
          let _v = _menhir_action_47 x in
          _menhir_goto_nonempty_list_simple_exp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_nonempty_list_simple_exp_ : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_simple_exp as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState096 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState101 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_103 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_simple_exp -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_simple_exp (_menhir_stack, _menhir_s, e1) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_02 e1 e2 in
      let _1 = _v in
      let _v = _menhir_action_27 _1 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_102 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_simple_exp, _menhir_box_main) _menhir_cell1_simple_exp -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_simple_exp (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_48 x xs in
      _menhir_goto_nonempty_list_simple_exp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_100 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_simple_exp, _menhir_box_main) _menhir_cell1_QUESTION, _menhir_box_main) _menhir_cell1_simple_exp -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_simple_exp (_menhir_stack, _, e1) = _menhir_stack in
      let MenhirCell1_QUESTION (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_simple_exp (_menhir_stack, _menhir_s, cond) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_08 cond e1 e2 in
      _menhir_goto_cond_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_098 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_simple_exp, _menhir_box_main) _menhir_cell1_QUESTION as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _menhir_s = MenhirState099 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT_VAL ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_096 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState096
      | UNIT_VAL ->
          let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState096
      | QUESTION ->
          let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
          let _menhir_stack = MenhirCell1_QUESTION (_menhir_stack, MenhirState096) in
          let _menhir_s = MenhirState097 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT_VAL ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | L_PAR ->
          let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState096
      | INT _v_4 ->
          let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState096
      | BOOL _v_5 ->
          let _menhir_stack = MenhirCell1_simple_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState096
      | AND | DIV | ELSE | END | ENDIF | EQUALS | GREATER | IN | LESSER | MINUS | MULT | OR | PLUS | R_PAR | SEMI_COLON | THEN ->
          let _1 = _v in
          let _v = _menhir_action_14 _1 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_214 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
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
                      let _menhir_s = MenhirState218 in
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      (match (_tok : MenhirBasics.token) with
                      | WAIT ->
                          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | VAR _v ->
                          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                      | SEND_CHAN ->
                          _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | SEND ->
                          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | PRINT ->
                          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | IF ->
                          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | FWD ->
                          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | CLOSE ->
                          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | CASE ->
                          _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
  
  and _menhir_run_052 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_stype (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState053 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | R_BRACE ->
          let MenhirCell1_VAR (_menhir_stack, _menhir_s, v) = _menhir_stack in
          let st = _v in
          let _v = _menhir_action_36 st v in
          _menhir_goto_lin_ctxt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_050 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _menhir_s = MenhirState051 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | T_VAR _v ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TUNIT ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TNUM ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TBOOL ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | S_VAR _v ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REC ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_PAR ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_BRACE ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | END_STYPE ->
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMPERSAND ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_lin_ctxt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState049 ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState053 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_055 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_BRACE, _menhir_box_main) _menhir_cell1_stype -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_stype (_menhir_stack, _, s) = _menhir_stack in
      let MenhirCell1_L_BRACE (_menhir_stack, _menhir_s) = _menhir_stack in
      let ctxt = _v in
      let _v = _menhir_action_74 ctxt s in
      _menhir_goto_simple_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_054 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_stype -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_stype (_menhir_stack, _, st) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, v) = _menhir_stack in
      let cl = _v in
      let _v = _menhir_action_37 cl st v in
      _menhir_goto_lin_ctxt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_047 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_BRACE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | R_BRACE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_L_BRACE (_menhir_stack, _menhir_s) = _menhir_stack in
          let s = _v in
          let _v = _menhir_action_73 s in
          _menhir_goto_simple_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | LEFT_ARROW ->
          let _menhir_stack = MenhirCell1_stype (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState049 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_046 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_REC _menhir_cell0_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_VAR (_menhir_stack, v) = _menhir_stack in
      let MenhirCell1_REC (_menhir_stack, _menhir_s) = _menhir_stack in
      let sty = _v in
      let _v = _menhir_action_78 sty v in
      _menhir_goto_stype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_041 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_stype (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState042 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
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
      | MenhirState015 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState042 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState021 ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_044 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_PLUS -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_PLUS (_menhir_stack, _menhir_s) = _menhir_stack in
      let l = _v in
      let _v = _menhir_action_83 l in
      _menhir_goto_stype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_043 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_stype -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_stype (_menhir_stack, _, st) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, v) = _menhir_stack in
      let cl = _v in
      let _v = _menhir_action_06 cl st v in
      _menhir_goto_choice_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_022 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_AMPERSAND -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_AMPERSAND (_menhir_stack, _menhir_s) = _menhir_stack in
      let l = _v in
      let _v = _menhir_action_82 l in
      _menhir_goto_stype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_040 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_stype -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_stype (_menhir_stack, _, st1) = _menhir_stack in
      let MenhirCell1_L_PAR (_menhir_stack, _menhir_s) = _menhir_stack in
      let st2 = _v in
      let _v = _menhir_action_85 st1 st2 in
      _menhir_goto_stype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_038 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_stype -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_stype (_menhir_stack, _, st1) = _menhir_stack in
      let MenhirCell1_L_PAR (_menhir_stack, _menhir_s) = _menhir_stack in
      let st2 = _v in
      let _v = _menhir_action_84 st1 st2 in
      _menhir_goto_stype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_035 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_stype (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | R_PAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | MULT ->
              let _menhir_s = MenhirState037 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | T_VAR _v ->
                  _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TUNIT ->
                  _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | TNUM ->
                  _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | TBOOL ->
                  _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | S_VAR _v ->
                  _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | REC ->
                  _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PLUS ->
                  _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | L_PAR ->
                  _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | L_BRACE ->
                  _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | END_STYPE ->
                  _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | AMPERSAND ->
                  _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | LOLLIPOP ->
              let _menhir_s = MenhirState039 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | T_VAR _v ->
                  _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TUNIT ->
                  _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | TNUM ->
                  _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | TBOOL ->
                  _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | S_VAR _v ->
                  _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | REC ->
                  _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PLUS ->
                  _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | L_PAR ->
                  _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | L_BRACE ->
                  _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | END_STYPE ->
                  _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | AMPERSAND ->
                  _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_034 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_cell1_RIGHT_ARROW_BOLD -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_RIGHT_ARROW_BOLD (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_ty (_menhir_stack, _menhir_s, t) = _menhir_stack in
      let st = _v in
      let _v = _menhir_action_80 st t in
      _menhir_goto_stype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_029 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_ty, _menhir_box_main) _menhir_cell1_CIRCUMFLEX -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_CIRCUMFLEX (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_ty (_menhir_stack, _menhir_s, t) = _menhir_stack in
      let st = _v in
      let _v = _menhir_action_79 st t in
      _menhir_goto_stype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_nonempty_list_fun_binder_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState131 ->
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState079 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_132 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_fun_binder -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_fun_binder (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_46 x xs in
      _menhir_goto_nonempty_list_fun_binder_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_090 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_FUN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_nonempty_list_fun_binder_ (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState091 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT_VAL ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_082 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let t = _v in
      let _v = _menhir_action_32 t x in
      _menhir_goto_fun_binder _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_033 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_fun_ty_list -> _ -> _ -> _ -> _ -> _menhir_box_main =
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
          let _v = _menhir_action_88 l t in
          _menhir_goto_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_fun_ty_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_fun_ty_list (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState032 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_VAR _v ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TUNIT ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TNUM ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TBOOL ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_BRACE ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_ty : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState226 ->
          _menhir_run_227 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState207 ->
          _menhir_run_208 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState002 ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState007 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState230 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState008 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState013 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState017 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState039 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState028 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState026 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState018 ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_227 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_TYPE _menhir_cell0_T_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
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
  
  and _menhir_run_208 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RIGHT_ARROW_BOLD ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState208
      | LEFT_ARROW ->
          let _menhir_stack = MenhirCell1_LEFT_ARROW (_menhir_stack, MenhirState208) in
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
                      let _menhir_s = MenhirState212 in
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      (match (_tok : MenhirBasics.token) with
                      | WAIT ->
                          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | VAR _v ->
                          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                      | SEND_CHAN ->
                          _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | SEND ->
                          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | PRINT ->
                          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | IF ->
                          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | FWD ->
                          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | CLOSE ->
                          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | CASE ->
                          _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | _ ->
                          _eRR ())
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | CIRCUMFLEX ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState208
      | _ ->
          _eRR ()
  
  and _menhir_run_026 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_ty as 'stack) -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_RIGHT_ARROW_BOLD (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState026 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | T_VAR _v ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TUNIT ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TNUM ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TBOOL ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | S_VAR _v ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REC ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_BRACE ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | END_STYPE ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMPERSAND ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_028 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_ty as 'stack) -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_CIRCUMFLEX (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState028 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | T_VAR _v ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TUNIT ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TNUM ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TBOOL ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | S_VAR _v ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REC ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_PAR ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_BRACE ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | END_STYPE ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMPERSAND ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_146 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState146
      | UNIT_VAL ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | L_PAR ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | LET ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | INT _v_1 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState146
      | IF ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | FUN ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | BOOL _v_2 ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState146
      | _ ->
          _eRR ()
  
  and _menhir_run_143 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_L_PAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState143
      | UNIT_VAL ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState143
      | R_PAR ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState143
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState143
      | L_PAR ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState143
      | LET ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState143
      | INT _v_1 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState143
      | IF ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState143
      | FUN ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState143
      | BOOL _v_2 ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState143
      | _ ->
          _eRR ()
  
  and _menhir_run_025 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR, _menhir_box_main) _menhir_cell1_ty -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_ty (_menhir_stack, _, t) = _menhir_stack in
      let MenhirCell1_L_PAR (_menhir_stack, _menhir_s) = _menhir_stack in
      let _v = _menhir_action_75 t in
      _menhir_goto_simple_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_058 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState058
      | UNIT_VAL ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState058
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState058
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState058
      | L_PAR ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState058
      | LET ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState058
      | INT _v_1 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState058
      | IF ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState058
      | FUN ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState058
      | BOOL _v_2 ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState058
      | _ ->
          _eRR ()
  
  and _menhir_run_057 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | R_PAR ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_027 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RIGHT_ARROW_BOLD ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState027
      | CIRCUMFLEX ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState027
      | _ ->
          _eRR ()
  
  and _menhir_run_024 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_L_PAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | R_PAR ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RIGHT_ARROW_BOLD ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | CIRCUMFLEX ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | _ ->
          _eRR ()
  
  and _menhir_run_030 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RIGHT_ARROW ->
          let _1 = _v in
          let _v = _menhir_action_34 _1 in
          _menhir_goto_fun_ty_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _ | CIRCUMFLEX | FUN | IF | INT _ | LEFT_ARROW | LET | L_PAR | MINUS | NOT | RIGHT_ARROW_BOLD | R_PAR | SEMI_COLON | UNIT_VAL | VAR _ ->
          let _1 = _v in
          let _v = _menhir_action_87 _1 in
          _menhir_goto_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  let _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState000
      | TYPE ->
          _menhir_run_225 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | STYPE ->
          _menhir_run_229 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | RETURN ->
          let _v = _menhir_action_40 () in
          _menhir_run_234 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState000
      | _ ->
          _eRR ()
  
end

let main =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_main v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

# 153 "lib/parser.mly"
  

# 5338 "lib/parser.ml"
