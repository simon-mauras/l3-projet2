type cnf = int * int * int list list
type certificate = int list
type solution = certificate option

module Literal =
  struct
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
  end

module type Formula_type =
  sig
    type t
    val make: out_channel -> cnf -> t
    val print: out_channel -> t -> unit
    
    val setLiteral: Literal.t -> t -> unit
    val forgetLiteral: Literal.t -> t -> unit
    
    val isFalse: t -> bool
    val getUnitClause: t -> Literal.t option
    val getPureLiteral: t -> Literal.t option
    val getFreeLiteral: t -> Literal.t option 
  end


module type Solver_type =
  functor (F : Formula_type) ->
    sig
      val solve: out_channel -> F.t -> solution
    end
