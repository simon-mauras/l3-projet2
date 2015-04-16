(** Module implémentant la theorie par défaut *)

(** Module de type Sigs.Theory_type *)
module Make : Sigs.Theory_type =
  struct

    (** Module représentant un terme de la théorie par défaut *)
    module T = struct
    
      type t = string
      
      let make x =
        let aux_parsing = function
        | Sigs.Parsing_default s -> s
        | _ -> failwith "Wrong parser" in
        let rec aux_formula = function
        | Sigs.And(a, b) -> Sigs.And(aux_formula a, aux_formula b)
        | Sigs.Or(a, b) -> Sigs.Or(aux_formula a, aux_formula b)
        | Sigs.Imp(a, b) -> Sigs.Imp(aux_formula a, aux_formula b)
        | Sigs.Not(a) -> Sigs.Not(aux_formula a)
        | Sigs.Atom(a) -> Sigs.Atom(aux_parsing a) in
        aux_formula x
      
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

