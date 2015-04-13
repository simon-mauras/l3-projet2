{
open Parser_diff;;
}

rule main = parse
  | [' ' '\t' '\n']              { main lexbuf }
  | '-' ? ['0'-'9']+ as s        { INT (int_of_string s) }
  | ['a'-'z' 'A'-'Z' '_']+ as s  { IDENT s }
  | "<"                          { LT }
  | ">"                          { GT }
  | "<="                         { LEQ }
  | ">="                         { GEQ }
  | "="                          { EQ }
  | "!="                         { NEQ }
  | "\\/"                        { OR }
  | "/\\"                        { AND }
  | "=>"                         { IMP }
  | "~"                          { NOT }
  | "("                          { LPAREN }
  | ")"                          { RPAREN }
  | eof                          { EOF }

