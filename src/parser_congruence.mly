%{
open Sigs
%}

%token <string> IDENT
%token EQ
%token NEQ
%token OR
%token AND
%token IMP
%token NOT
%token LPAREN
%token RPAREN
%token EOF

%left IMP
%left OR AND
%nonassoc NOT

%type <Sigs.Congruence.t Sigs.formula> main

%start main
%%

main:
  | f = formula EOF { f }
;

formula:
  | LPAREN f = formula RPAREN
      { f }
  | f1 = formula OR f2 = formula
      { Or (f1, f2) }
  | f1 = formula AND f2 = formula
      { And (f1, f2) }
  | f1 = formula IMP f2 = formula
      { Imp (f1, f2) }
  | NOT f = formula
      { Not f }
  | a = atom { Atom a }
;

atom:
  | t1 = term EQ t2 = term
      { Congruence.Eq (t1, t2) }
  | t1 = term NEQ t2 = term
      { Congruence.Neq (t1, t2) }
;

term:
  | f = IDENT LPAREN lst = term_list RPAREN
      { Congruence.Fun (f, lst) }
  | x = IDENT
      { Congruence.X x }
;

term_list:
  | s = IDENT
      { [ Congruence.X s ] }
  | s = IDENT lst = term_list
      { (Congruence.X s)::lst }
;
