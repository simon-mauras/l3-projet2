{
open Parser_equality;;
}

rule main = parse
  | [' ' '\t' '\n']              { main lexbuf }
  | ['a'-'z' 'A'-'Z' '0'-'9' '_']+ as s  { IDENT s }
  | "="                          { EQ }
  | "!="                         { NEQ }
  | "\\/"                        { OR }
  | "/\\"                        { AND }
  | "=>"                         { IMP }
  | "~"                          { NOT }
  | "("                          { LPAREN }
  | ")"                          { RPAREN }
  | eof                          { EOF }

