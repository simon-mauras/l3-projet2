%token <int> INT
%token END
%token EOF
%token P
%token CNF

%start formula
%type <int * int * int list list> formula
%%

formula:
  | P CNF INT INT cnf       { ($3, $4, $5) }
;

cnf:
  | EOF                     { [] }
  | clause cnf              { $1::$2 }
;

clause:
  | END                     { [] }
  | INT clause              { $1::$2 }
;
