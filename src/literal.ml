module Make: Sigs.Literal =
  struct
    (* Type of a literal *)
    type t = X of int | Xbar of int
    
    (* Constructor *)
    let make x = if x < 0 then Xbar(-x) else X(x)
    
    (* Print a literal on output "out" *)
    let print out = function
      | X x    -> output_string out "X(";    output_string out (string_of_int x); output_string out ")"
      | Xbar x -> output_string out "Xbar("; output_string out (string_of_int x); output_string out ")"
      
    (* Compare 2 literals *)
    let compare a b = match a, b with
      | X xa, Xbar xb    when xa = xb -> 1
      | Xbar xa, X xb    when xa = xb -> -1
      | X xa, X xb       when xa = xb -> 0
      | Xbar xa, Xbar xb when xa = xb -> 0
      | X xa, Xbar xb | Xbar xa, X xb | X xa, X xb | Xbar xa, Xbar xb -> if xa < xb then 1 else -1
      
    (* Return the negation of a literal *)
    let neg = function
      | X x -> Xbar x
      | Xbar x -> X x
      
    (* Return an id beetween 0 and 2*nb_vars - 1 *)
    let id_of_literal = function
      | X x -> 2*(x-1)
      | Xbar x -> 2*(x-1) + 1
    
    (* Return the corresponding literal of an id *)
    let literal_of_id n =
      if n mod 2 = 0
        then X (1+n/2)
        else Xbar (1+n/2)
  end