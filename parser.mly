%{
open Expr
%}

%token <int> INT
%token END
%token EOF
%token P
%token CNF

%start formula
%type <Expr.formula> formula
%%

formula:
  | P CNF INT INT cnf       { Cnf($3, $4, $5) }
;

cnf:
  | EOF                     { [] }
  | clause cnf              { $1::$2 }
;

clause:
  | END                     { [] }
  | INT clause              { (if $1 > 0 then X($1) else Xbar(-$1))::$2 }
;
