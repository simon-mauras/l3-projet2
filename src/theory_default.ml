(** Module implémentant la theorie par défaut *)

(** Module de type Sigs.Theory_type *)
module Make : Sigs.Theory_type =
  struct

    (** Module représentant un terme de la théorie par défaut *)
    module T = struct
      type t = string
      let make = function
        | Sigs.Parsing_default s -> s
        | _ -> failwith "Wrong parser"
      let compare = String.compare
      let print output = output_string output
    end
    
    (** Type d'un terme de la logique par défaut *)
    type term = string
    
    (** Type d'un ensemble de contraintes *)
    type t = unit
    
    (** Un ensemble de contraintes vide *)
    let make _ = ()
    
    (** Ajoute une contrainte *)
    let setConstraint l e = ()
    
    (** Oublie une contrainte *)
    let forgetConstraint l e = ()
    
    (** Renvoie une eventuelle contradiction entre les contraintes actuelles *)
    let getContradiction e = None
end

