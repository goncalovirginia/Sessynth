%{
  open InputSyntax
%}

%token <string> VAR
%token <string> T_VAR
%token <string> S_VAR
%token <int> INT
%token <bool> BOOL 
%token LET IN END
%token RETURN
%token AND OR EQUALS NOT 
%token PLUS MULT DIV MINUS LESSER GREATER
%token FUN RIGHT_ARROW L_PAR R_PAR UNIT_VAL
%token TNUM TBOOL COLON TUNIT
%token IF THEN ELSE ENDIF QUESTION
%token LEFT_ARROW L_BRACE R_BRACE SEMI_COLON COMMA
%token SEND RECV CLOSE WAIT FWD SPAWN SEND_CHAN RECV_CHAN PRINT
%token CASE OF DOT
%token END_STYPE RIGHT_ARROW_BOLD CIRCUMFLEX AMPERSAND LOLLIPOP
%token REC TYPE STYPE
%token V_BAR
%token EOF

%left EQUALS
%left PLUS MINUS
%left AND OR
%left GREATER LESSER
%left MULT DIV

%right RIGHT_ARROW

%start main 
%type <InputSyntax.prog> main

%%
main: 
  | d = declaration* RETURN e = exec_exp RETURN EOF   { Prog (d, e) }
 
declaration:
  | STYPE v = S_VAR st = stype SEMI_COLON             { Decl (v, Annot (Var v, TProc(st, []))) }
  | TYPE v = T_VAR t = ty SEMI_COLON                  { Decl (v, Annot (Var v, t))}
  | var = VAR COLON t = ty e = expression SEMI_COLON  { Decl (var, Annot (e, t)) } // Expression type declaration

expression:
  | simple_exp                                                        { $1 }
  | FUN l = fun_binder+ RIGHT_ARROW body = expression END             { FunDef (l, body) } 
  | LET var = VAR EQUALS e1 = expression IN e2 = expression END       { Let (var, e1, e2) }  
  | e1 = expression op = bop e2 = expression                          { BOp (op, e1, e2) }
  | op = uop e = simple_exp                                           { UOp (op, e) }
  | app_exp                                                           { $1 }
  | annot_exp                                                         { $1 }
  | cond_exp                                                          { $1 }
  | proc_exp                                                          { $1 }
  | synth_exp                                                         { $1 }

exec_exp:
  | e = proc_exp    { ExecExp e }
  | e = simple_exp  { ExecExp e }  
  | e = synth_exp   { ExecExp e }

proc_exp:
  | c = VAR LEFT_ARROW L_BRACE p = proc R_BRACE { ProcExp (c, p) }

proc:
  | SEND c = VAR e = expression SEMI_COLON p = proc                                     { Send (c, e, p) }
  | id = VAR COLON t = ty LEFT_ARROW RECV c = VAR SEMI_COLON p = proc                   { Recv (id, c, Some t, p) } // x:t <- recv c ; P    
  | id = VAR LEFT_ARROW RECV c = VAR SEMI_COLON p = proc                                { Recv (id, c, None, p) } // x <- recv c ; P    
  | CLOSE c = VAR                                                                       { Close (c) }
  | WAIT c = VAR SEMI_COLON p = proc                                                    { Wait (c, p) }
  | FWD c1 = VAR c2 = VAR                                                               { Fwd (c1, c2) }
  | c = VAR LEFT_ARROW SPAWN L_PAR e = expression R_PAR args = VAR* SEMI_COLON p = proc { Spawn (c, e, p, args) }
  | c = VAR LEFT_ARROW SPAWN L_BRACE p1 = proc R_BRACE args = VAR* SEMI_COLON p2 = proc { Spawn (c, ProcExp(c, p1), p2, args) }
  | CASE c = VAR OF l = case_list                                                       { Choice (c, l) }
  | c = VAR DOT l = VAR SEMI_COLON p = proc                                             { Label (c, l, p) }
  | SEND_CHAN c = VAR e = VAR SEMI_COLON p = proc                                       { SendChan (c, e, p) }
  | id = VAR COLON st = stype LEFT_ARROW RECV_CHAN c = VAR SEMI_COLON p = proc          { RecvChan (id, c, Some st, p) }
  | id = VAR LEFT_ARROW RECV_CHAN c = VAR SEMI_COLON p = proc                           { RecvChan (id, c, None, p) } 
  | PRINT e = expression SEMI_COLON p = proc                                            { Print (e, p) }
  | IF e = expression THEN p1 = proc ELSE p2 = proc                                     { If (e, p1, p2) }

stype:
  | v = VAR                                      { STVar (v) }  // Recursive variable only
  | REC v = VAR DOT sty = stype                  { STRec (v, sty) } // Recursive type only
  | t = ty CIRCUMFLEX st = stype                 { STSend (t, st) }
  | t = ty RIGHT_ARROW_BOLD st = stype           { STRecv (t, st) }
  | END_STYPE                                    { STEnd }
  | AMPERSAND L_BRACE l = choice_list R_BRACE    { STExtChoice (l) }
  | PLUS L_BRACE l = choice_list R_BRACE         { STIntChoice (l) }
  | L_PAR st1 = stype R_PAR MULT st2 = stype     { STSendChan (st1, st2) }
  | L_PAR st1 = stype R_PAR LOLLIPOP st2 = stype { STRecvChan (st1, st2) }
  | v = S_VAR                                    { STUVar (v) }

choice_list:
  | v = VAR COLON st = stype                        { [(v, st)] }
  | v = VAR COLON st = stype COMMA cl = choice_list { (v,st)::cl }

case_list:
  | l = VAR COLON L_PAR p = proc R_PAR                 { [(l, p)] }
  | l = VAR COLON L_PAR p = proc R_PAR cl = case_list  { (l, p)::cl }

cond_exp:
  | IF cond = expression THEN e1 = expression ELSE e2 = expression ENDIF { Cond (cond, e1, e2) }
  | cond = simple_exp QUESTION e1 = simple_exp COLON e2 = simple_exp     { Cond (cond, e1, e2) }

annot_exp:
  | L_PAR t = ty e = expression R_PAR { Annot (e, t) }

app_exp:
  | e1 = simple_exp  e2 = simple_exp+ { FunApp (e1, e2) }  

simple_exp:
  | UNIT_VAL                        { UnitVal }
  | i = INT                         { Num (i) }
  | b = BOOL                        { Bool (b) }
  | v = VAR                         { Var (v) }
  | L_PAR e = expression R_PAR      { e }

fun_binder:
  | x = VAR                                   { ([x], None) }
  | x = VAR COLON t = simple_ty               { ([x], Some t) }
  | L_PAR lx = VAR+ COLON t = simple_ty R_PAR { (lx, Some t) }

ty:
  | simple_ty                                  { $1 }
  | l = fun_ty_list RIGHT_ARROW t = simple_ty  { TFun(l, t) }
  
simple_ty:
  | TUNIT                                                 { TUnit }
  | TNUM                                                  { TNum }
  | TBOOL                                                 { TBool }        
  | L_BRACE s = stype R_BRACE                             { TProc (s, [])}     
  | L_BRACE s = stype LEFT_ARROW ctxt = lin_ctxt R_BRACE  { TProc (s, ctxt)}     
  | L_PAR t = ty R_PAR                                    { t }
  | x = T_VAR                                             { TVar x }

lin_ctxt:
  | v = VAR COLON st = stype                     { [(v, st)] }
  | v = VAR COLON st = stype COMMA cl = lin_ctxt { (v,st)::cl }

fun_ty_list:
  | simple_ty                                 { [$1] }
  | l = fun_ty_list RIGHT_ARROW t = simple_ty { l@[t] }

uop:
  | MINUS { Neg }
  | NOT   { Not }

%inline bop:
  | MULT      { Mul }
  | DIV       { Div }
  | AND       { And }
  | OR        { Or }
  | PLUS      { Add }
  | MINUS     { Sub }
  | LESSER    { Lesser }
  | GREATER   { Greater }
  | EQUALS    { Equals }

(* sessynth parsing *)

synth_exp:
  | QUESTION t = sessynth_tyF QUESTION    { Synth(t) }

sessynth_tyF:
  | t = sessynth_tyA                                                              { Sessynth.Language.TAtomic(t) }
  | L_BRACE var = VAR COLON tA = sessynth_tyA V_BAR tR = sessynth_tyR R_BRACE     { Sessynth.Language.TRefinement(var, tA, tR) }
  | var = VAR COLON tA = sessynth_tyA                                             { Sessynth.Language.TRefinement(var, tA, Sessynth.Language.RTBool(true)) }
  | t1 = sessynth_tyF RIGHT_ARROW t2 = sessynth_tyF                               { Sessynth.Language.TArrow(t1, t2) }
  | L_BRACE outs = sessynth_tyS R_BRACE                                           { Sessynth.Language.TProcess([], outs) }     
  | L_BRACE outs = sessynth_tyS LEFT_ARROW incsl = sessynth_tyS_list R_BRACE      { Sessynth.Language.TProcess(incsl, outs) }
  | var = VAR                                                                     { Sessynth.Language.TDeclr(var) }

sessynth_tyR:
  | sessynth_simple_tyR                                           { $1 }
  | t1 = sessynth_tyR op = sessynth_tyR_bop t2 = sessynth_tyR     { Sessynth.Language.RTBOp(op, t1, t2) }
  | op = sessynth_tyR_uop t = sessynth_simple_tyR                 { Sessynth.Language.RTUOp(op, t) }

sessynth_simple_tyR:
  | i = INT                                                       { Sessynth.Language.RTInt(i) }
  | b = BOOL                                                      { Sessynth.Language.RTBool(b) }
  | var = VAR                                                     { Sessynth.Language.RTVar(var) }

sessynth_tyR_uop:
  | NOT     { Sessynth.Language.Not }
  | MINUS   { Sessynth.Language.Neg }

%inline sessynth_tyR_bop:
  | AND               { Sessynth.Language.And }
  | OR                { Sessynth.Language.Or }
  | GREATER EQUALS    { Sessynth.Language.GrE }
  | LESSER EQUALS     { Sessynth.Language.LtE }
  | EQUALS            { Sessynth.Language.Eq }
  | GREATER           { Sessynth.Language.Gr }
  | LESSER            { Sessynth.Language.Lt }
  | PLUS              { Sessynth.Language.Sum }
  | MINUS             { Sessynth.Language.Sub }
  | MULT              { Sessynth.Language.Mult }
  | DIV               { Sessynth.Language.Div }

sessynth_tyA:
  | TNUM    { Sessynth.Language.TInt }
  | TBOOL   { Sessynth.Language.TBool }

sessynth_tyS:
  | t1 = sessynth_tyF CIRCUMFLEX t2 = sessynth_tyS            { Sessynth.Language.STSendF(t1, t2) }
  | t1 = sessynth_tyF RIGHT_ARROW_BOLD t2 = sessynth_tyS      { Sessynth.Language.STRecvF(t1, t2) }
  | L_PAR t1 = sessynth_tyS R_PAR MULT t2 = sessynth_tyS      { Sessynth.Language.STSendS(t1, t2) }
  | L_PAR t1 = sessynth_tyS R_PAR LOLLIPOP t2 = sessynth_tyS  { Sessynth.Language.STRecvS(t1, t2) }
  | END_STYPE                                                 { Sessynth.Language.STUnit }
  | AMPERSAND L_BRACE l = sessynth_choice_list R_BRACE        { Sessynth.Language.STExtChoice(l) }
  | PLUS L_BRACE l = sessynth_choice_list R_BRACE             { Sessynth.Language.STIntChoice(l) }
  | REC v = VAR DOT st = sessynth_tyS                         { Sessynth.Language.STRec(v, st) }
  | v = S_VAR                                                 { Sessynth.Language.STRecVar(v) }

sessynth_tyS_list:
  | t = sessynth_tyS                                { [("_", t)] }
  | t = sessynth_tyS COMMA tl = sessynth_tyS_list   { ("_", t)::tl }

sessynth_choice_list:
  | v = VAR COLON st = sessynth_tyS                                   { [(v, st)] }
  | v = VAR COLON st = sessynth_tyS COMMA cl = sessynth_choice_list   { (v,st)::cl }

%%
