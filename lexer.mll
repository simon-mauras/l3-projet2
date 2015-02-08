{
open Parser;;
}

rule main = parse
  | [' ' '\t' '\n']         { main lexbuf }
  | ['c']                   { comment lexbuf }
  | "-" ? ['0'-'9']+ as s   { let i = int_of_string s in
                              if i = 0 then END else INT i }
  | "cnf"                   { CNF }
  | "p"                     { P }
  | eof                     { EOF }

and comment = parse
  | '\n'                    { main lexbuf }
  | eof                     { EOF }
  | _                       { comment lexbuf }
