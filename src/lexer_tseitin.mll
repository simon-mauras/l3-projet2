{
open Parser_tseitin;;
}

rule main = parse
  | [' ' '\t' '\n']              { main lexbuf }
  | ['a'-'z' 'A'-'Z' '_']+ as s  { IDENT s }
  | "\\/"                        { OR }
  | "/\\"                        { AND }
  | "=>"                         { IMP }
  | "~"                          { NOT }
  | "("                          { LPAREN }
  | ")"                          { RPAREN }
  | eof                          { EOF }

