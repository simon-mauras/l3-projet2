module type Literal_type =
  sig
    type t
    val make: int -> t
    val compare: t -> t -> int
    val print: out_channel -> t -> unit
    val neg: t -> t
  end

module type Formula_type =
  sig
    type t
    val make: out_channel -> int * int * int list list -> t
    val print: out_channel -> t -> unit
  end
