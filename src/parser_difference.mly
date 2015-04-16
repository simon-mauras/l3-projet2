%{
open Sigs
open Sigs.Data.Difference
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

%type <Sigs.parsing Sigs.formula> main

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
  | a = atom { Atom (Parsing_difference a) }
;

atom:
  | t1 = term EQ n = INT
      { Binary (t1, Eq, n) }
  | t1 = term NEQ n = INT
      { Binary (t1, Neq, n) }
  | t1 = term LT n = INT
      { Binary (t1, Lt, n) }
  | t1 = term GT n = INT
      { Binary (t1, Gt, n) }
  | t1 = term LEQ n = INT
      { Binary (t1, Leq, n) }
  | t1 = term GEQ n = INT
      { Binary (t1, Geq, n) }
  | t1 = term MINUS t2 = term EQ n = INT
      { Ternary (t1, t2, Eq, n) }
  | t1 = term MINUS t2 = term NEQ n = INT
      { Ternary (t1, t2, Neq, n) }
  | t1 = term MINUS t2 = term GT n = INT
      { Ternary (t1, t2, Gt, n) }
  | t1 = term MINUS t2 = term LT n = INT
      { Ternary (t1, t2, Lt, n) }
  | t1 = term MINUS t2 = term GEQ n = INT
      { Ternary (t1, t2, Geq, n) }
  | t1 = term MINUS t2 = term LEQ n = INT
      { Ternary (t1, t2, Leq, n) }
;

term:
  | x = IDENT
      { X x }
;
