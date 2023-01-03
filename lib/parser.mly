%{
open Ast
%}

%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token PLUS
%token MINUS
%token MUL
%token EQ
%token LEQ
%token <string> ID
%token <string> CONST
%token SKIP
%token TAKES
%token SEQ
%token IF
%token THEN
%token ELSE

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LARR
%token RARR
%token EOF

%token PROC
%token REPEAT
%token FOREVER
%token BREAK

%token ARRAY
%token VAL
%token REF
%token INT

%left SEQ
%nonassoc ELSE
%left OR
%left AND
%nonassoc NOT
%left EQ LEQ
%left PLUS MINUS
%left MUL

%start <prog> prog

%%

prog:
  | d0 = dv; d1 = dp; c = cmd; EOF { Prog(d0,d1,c) }
;

(* Espressioni *)
expr:
  | n = CONST { Const(int_of_string n) }
  | e1=expr; PLUS; e2=expr { Add(e1,e2) }
  | e1=expr; MINUS; e2=expr { Sub(e1,e2) }
  | e1=expr; MUL; e2=expr { Mul(e1,e2) }
  | TRUE { True }
  | FALSE { False }
  | e1=expr; AND; e2=expr { And(e1,e2) }
  | e1=expr; OR; e2=expr { Or(e1,e2) }
  | NOT; e=expr { Not e }
  | e1=expr; EQ; e2=expr { Eq(e1,e2) }
  | e1=expr; LEQ; e2=expr { Leq(e1,e2) }
  | x = ID { Var(x) }
  | x = ID; LARR; e=expr; RARR { ArrVar(x,e) }
  | LPAREN; e = expr; RPAREN { e }

(* Dichiarazioni variabili e array *)
dv:
  | { NullVar }
  | d0 = dv; SEQ; d1 = dv { DVSeq(d0,d1) }
  | INT; x = ID { Var(x) }
  | ARRAY; x = ID; LARR; dim = expr; RARR { Array(x,dim) }

(* Comandi *)
cmd:
  | SKIP { Skip }
  | BREAK { Break }
  | x = ID; TAKES; e=expr; { Assign(x,e) }
  | x = ID; LARR; e0=expr; RARR; TAKES; e1=expr; { ArrayAssign(x,e0,e1) }
  | c1 = cmd; SEQ; c2 = cmd; { Seq(c1,c2) }
  | REPEAT; c=cmd; FOREVER { Repeat(c) }
  | IF; e0 = expr; THEN; c1 = cmd; ELSE; c2 = cmd; { If(e0,c1,c2) }
  | d = dv; SEQ; c = cmd { Block(d,c) }
  | LPAREN; c = cmd; RPAREN { c }

(* Dichiarazioni procedure *)
dp:
  | { NullProc }
  | d0 = dp; SEQ; d1 = dp { DPSeq(d0,d1) }
  | PROC; x = ID; LPAREN; param = pf; RPAREN; LBRACE; c = cmd; RBRACE { Proc(x,param,c) }

(* Parametri formali *)
pf:
  | VAL; x = ID { Val(x) }
  | REF; x = ID { Ref(x) }

(* Parametri attuali *)
pa:
  | e = expr { CurrentP(e) }
