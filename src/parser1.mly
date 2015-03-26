%{
open Sigs
%}

%token <char> CHAR
%token EOF
%token OR
%token AND
%token IMP
%token NOT
%token LPAREN
%token RPAREN

%type <char list Sigs.formula option> shell

%start shell
%%

shell:
  | EOF       { None }
  | f = formula EOF { Some f }
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
  | str = list(CHAR) { Atom str }
