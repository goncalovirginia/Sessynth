%start main
%token AMPERSAND
%token AND
%token <bool> BOOL
%token CASE
%token CIRCUMFLEX
%token CLOSE
%token COLON
%token COMMA
%token DIV
%token DOT
%token ELSE
%token END
%token ENDIF
%token END_STYPE
%token EOF
%token EQUALS
%token FUN
%token FWD
%token GREATER
%token IF
%token IMPORT
%token IN
%token <int> INT
%token LEFT_ARROW
%token LESSER
%token LET
%token LOLLIPOP
%token L_BRACE
%token L_PAR
%token MINUS
%token MODULE
%token MULT
%token NOT
%token OF
%token OR
%token PLUS
%token PRINT
%token QUESTION
%token REC
%token RECV
%token RECV_CHAN
%token RETURN
%token RIGHT_ARROW
%token RIGHT_ARROW_BOLD
%token R_BRACE
%token R_PAR
%token SEMI_COLON
%token SEND
%token SEND_CHAN
%token SPAWN
%token STYPE
%token <string> S_VAR
%token TBOOL
%token THEN
%token TNUM
%token TUNIT
%token TYPE
%token <string> T_VAR
%token UNIT_VAL
%token <string> VAR
%token WAIT
%token WHERE
%left EQUALS
%left MINUS PLUS
%left AND OR
%left GREATER LESSER
%left DIV MULT
%type <unit> main
%%

list_VAR_:
  
    {} [@name nil_VAR]
| VAR list_VAR_
    {} [@name cons_VAR]

list_declaration_:
  
    {} [@name nil_declaration]
| declaration list_declaration_
    {} [@name cons_declaration]

list_imprt_:
  
    {} [@name nil_imprt]
| imprt list_imprt_
    {} [@name cons_imprt]

nonempty_list_VAR_:
  VAR
    {} [@name one_VAR]
| VAR nonempty_list_VAR_
    {} [@name more_VAR]

nonempty_list_fun_binder_:
  fun_binder
    {} [@name one_fun_binder]
| fun_binder nonempty_list_fun_binder_
    {} [@name more_fun_binder]

nonempty_list_simple_exp_:
  simple_exp
    {} [@name one_simple_exp]
| simple_exp nonempty_list_simple_exp_
    {} [@name more_simple_exp]

main:
  MODULE VAR WHERE list_imprt_ prog EOF
    {}

imprt:
  IMPORT VAR L_PAR nonempty_list_VAR_ R_PAR END
    {}

prog:
  list_declaration_ RETURN exec_exp RETURN
    {}

declaration:
  STYPE S_VAR stype SEMI_COLON
    {}
| TYPE T_VAR ty SEMI_COLON
    {}
| VAR COLON ty expression SEMI_COLON
    {}

expression:
  simple_exp
    {}
| FUN nonempty_list_fun_binder_ RIGHT_ARROW expression END
    {}
| LET VAR EQUALS expression IN expression END
    {}
| expression MULT expression
    {}
| expression DIV expression
    {}
| expression AND expression
    {}
| expression OR expression
    {}
| expression PLUS expression
    {}
| expression MINUS expression
    {}
| expression LESSER expression
    {}
| expression GREATER expression
    {}
| expression EQUALS expression
    {}
| uop simple_exp
    {}
| app_exp
    {}
| annot_exp
    {}
| cond_exp
    {}
| proc_exp
    {}

exec_exp:
  proc_exp
    {}
| simple_exp
    {}

proc_exp:
  VAR LEFT_ARROW L_BRACE proc R_BRACE
    {}

proc:
  SEND VAR expression SEMI_COLON proc
    {}
| VAR COLON ty LEFT_ARROW RECV VAR SEMI_COLON proc
    {}
| VAR LEFT_ARROW RECV VAR SEMI_COLON proc
    {}
| CLOSE VAR
    {}
| WAIT VAR SEMI_COLON proc
    {}
| FWD VAR VAR
    {}
| VAR LEFT_ARROW SPAWN L_PAR expression R_PAR list_VAR_ SEMI_COLON proc
    {}
| VAR LEFT_ARROW SPAWN L_BRACE proc R_BRACE list_VAR_ SEMI_COLON proc
    {}
| CASE VAR OF case_list
    {}
| VAR DOT VAR SEMI_COLON proc
    {}
| SEND_CHAN VAR VAR SEMI_COLON proc
    {}
| VAR COLON stype LEFT_ARROW RECV_CHAN VAR SEMI_COLON proc
    {}
| VAR LEFT_ARROW RECV_CHAN VAR SEMI_COLON proc
    {}
| PRINT expression SEMI_COLON proc
    {}
| IF expression THEN proc ELSE proc
    {}

stype:
  VAR
    {}
| REC VAR DOT stype
    {}
| ty CIRCUMFLEX stype
    {}
| ty RIGHT_ARROW_BOLD stype
    {}
| END_STYPE
    {}
| AMPERSAND L_BRACE choice_list R_BRACE
    {}
| PLUS L_BRACE choice_list R_BRACE
    {}
| L_PAR stype R_PAR MULT stype
    {}
| L_PAR stype R_PAR LOLLIPOP stype
    {}
| S_VAR
    {}

choice_list:
  VAR COLON stype
    {}
| VAR COLON stype COMMA choice_list
    {}

case_list:
  VAR COLON L_PAR proc R_PAR
    {}
| VAR COLON L_PAR proc R_PAR case_list
    {}

cond_exp:
  IF expression THEN expression ELSE expression ENDIF
    {}
| simple_exp QUESTION simple_exp COLON simple_exp
    {}

annot_exp:
  L_PAR ty expression R_PAR
    {}

app_exp:
  simple_exp nonempty_list_simple_exp_
    {}

simple_exp:
  UNIT_VAL
    {}
| INT
    {}
| BOOL
    {}
| VAR
    {}
| L_PAR expression R_PAR
    {}

fun_binder:
  VAR
    {}
| VAR COLON simple_ty
    {}
| L_PAR nonempty_list_VAR_ COLON simple_ty R_PAR
    {}

ty:
  simple_ty
    {}
| fun_ty_list RIGHT_ARROW simple_ty
    {}

simple_ty:
  TUNIT
    {}
| TNUM
    {}
| TBOOL
    {}
| L_BRACE stype R_BRACE
    {}
| L_BRACE stype LEFT_ARROW lin_ctxt R_BRACE
    {}
| L_PAR ty R_PAR
    {}
| T_VAR
    {}

lin_ctxt:
  VAR COLON stype
    {}
| VAR COLON stype COMMA lin_ctxt
    {}

fun_ty_list:
  simple_ty
    {}
| fun_ty_list RIGHT_ARROW simple_ty
    {}

uop:
  MINUS
    {}
| NOT
    {}

%%
