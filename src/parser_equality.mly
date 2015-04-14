%{
open Sigs
open Sigs.Data.Equality
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
  | a = term { Atom (Parsing_equality a) }
;

term:
  | t1 = atom EQ t2 = atom
      { Eq (t1, t2) }
  | t1 = atom NEQ t2 = atom
      { Neq (t1, t2) }
;

atom:
  | x = IDENT
      { X x }
;
