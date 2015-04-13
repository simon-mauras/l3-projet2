%{
open Sigs
%}

%token <int> INT
%token <string> IDENT
%token MINUS
%token EQ
%token NEQ
%token LT
%token GT
%token LEQ
%token GEQ
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

%type <Sigs.Difference.atom Sigs.formula> main

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
  | t1 = term EQ n = INT
      { Difference.Binary (t1, Difference.Eq, n) }
  | t1 = term NEQ n = INT
      { Difference.Binary (t1, Difference.Neq, n) }
  | t1 = term LT n = INT
      { Difference.Binary (t1, Difference.Lt, n) }
  | t1 = term GT n = INT
      { Difference.Binary (t1, Difference.Gt, n) }
  | t1 = term LEQ n = INT
      { Difference.Binary (t1, Difference.Leq, n) }
  | t1 = term GEQ n = INT
      { Difference.Binary (t1, Difference.Geq, n) }
  | t1 = term MINUS t2 = term EQ n = INT
      { Difference.Ternary (t1, t2, Difference.Eq, n) }
  | t1 = term MINUS t2 = term NEQ n = INT
      { Difference.Ternary (t1, t2, Difference.Neq, n) }
  | t1 = term MINUS t2 = term GT n = INT
      { Difference.Ternary (t1, t2, Difference.Gt, n) }
  | t1 = term MINUS t2 = term LT n = INT
      { Difference.Ternary (t1, t2, Difference.Lt, n) }
  | t1 = term MINUS t2 = term GEQ n = INT
      { Difference.Ternary (t1, t2, Difference.Geq, n) }
  | t1 = term MINUS t2 = term LEQ n = INT
      { Difference.Ternary (t1, t2, Difference.Leq, n) }
;

term:
  | x = IDENT
      { Difference.X x }
;
