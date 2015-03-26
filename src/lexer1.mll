{
open Parser1
exception SyntaxError of string
}

let white = [' ' '\t']
let newline = '\r'|'\n'|"\r\n"

let lparen = '('
let rparen = ')'

let not_ = "~"
let or = "\\/"
let and_ = "/\\"
let imp = "=>"

rule read = parse
    | white { read lexbuf }
    | newline { read lexbuf }
    | eof { EOF } 
    | lparen { LPAREN }
    | rparen { RPAREN }
    | not_ { NOT }
    | or { OR }
    | and_ { AND }
    | imp { IMP }
    | _ as s { CHAR s }

