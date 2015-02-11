type t = X of int | Xbar of int

let make x = if x < 0 then Xbar(-x) else X(x)

let print out = function
  | X x    -> output_string out "X(";    output_string out (string_of_int x); output_string out ")"
  | Xbar x -> output_string out "Xbar("; output_string out (string_of_int x); output_string out ")"

let compare a b = match a, b with
  | X xa, Xbar xb    when xa = xb -> 1
  | Xbar xa, X xb    when xa = xb -> -1
  | X xa, X xb       when xa = xb -> 0
  | Xbar xa, Xbar xb when xa = xb -> 0
  | X xa, Xbar xb | Xbar xa, X xb | X xa, X xb | Xbar xa, Xbar xb -> if xa < xb then 1 else -1

let neg = function
  | X x -> Xbar x
  | Xbar x -> X x

