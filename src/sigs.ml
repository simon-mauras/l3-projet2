type cnf = int * int * int list list
type certificate = int list
type solution = certificate option

module type Literal =
sig
  type t = X of int | Xbar of int
  val make : int -> t
  val print : out_channel -> t -> unit
  val compare : t -> t -> int
  val neg : t -> t
  val id_of_literal : t -> int
  val literal_of_id : int -> t
end


module type Formula_type =
sig
  type t
  module Literal : Literal

  val make: out_channel -> cnf -> t
  val print: out_channel -> t -> unit

  val setLiteral: Literal.t -> t -> unit
  val forgetLiteral: Literal.t -> t -> unit

  val isFalse: t -> bool
  val isTrue: t -> bool
  val getUnitClause: t -> Literal.t option
  val getPureLiteral: t -> Literal.t option
  val getFreeLiteral: t -> Literal.t option 
end

module type Solver_type =
  functor (F : Formula_type) ->
  sig
    val solve: out_channel -> F.t -> solution
  end

