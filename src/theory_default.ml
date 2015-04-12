(** Module implémentant la theorie par défaut *)

(** Module de type Sigs.Theory_type *)
module Make : Sigs.Theory_type =
  functor (L : Sigs.Literal_type) ->
  struct

    (** Module implémentant la manipulation de litéraux *)
    module Literal = L
    
    (** Type d'un ensemble de contraintes *)
    type t = unit
    
    (** Un ensemble de contraintes vide *)
    let empty = ()
    
    (** Ajoute une contrainte *)
    let setConstraint l e = ()
    
    (** Oublie une contrainte *)
    let forgetConstraint l e = ()
    
    (** Renvoie une eventuelle contradiction entre les contraintes actuelles *)
    let getContradiction e = None
end

