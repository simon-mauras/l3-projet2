(** Module implementant la theorie par defaut *)

open Sigs

(** Module de type Theory_type *)
module Make : Theory_type =
  struct

    (** Module representant un terme de la theorie par defaut *)
    module T = struct
    
      type t = string
      
      let make x =
        let aux_parsing = function
        | Parsing_default s -> s
        | _ -> failwith "Wrong parser" in
        let rec aux_formula = function
        | And(a, b) -> And(aux_formula a, aux_formula b)
        | Or(a, b) -> Or(aux_formula a, aux_formula b)
        | Imp(a, b) -> Imp(aux_formula a, aux_formula b)
        | Not(a) -> Not(aux_formula a)
        | Atom(a) -> Atom(aux_parsing a) in
        aux_formula x
      
      let compare = String.compare
      
      let print output = output_string output
      
    end
    
    (** Type d'un terme de la logique par defaut *)
    type term = string
    
    (** Type d'un ensemble de contraintes *)
    type t = unit
    
    (** Un ensemble de contraintes vide *)
    let make _ : t = ()
    
    (** Ajoute une contrainte *)
    let setConstraint l e = ()
    
    (** Oublie une contrainte *)
    let forgetConstraint l e = ()
    
    (** Renvoie une eventuelle contradiction entre les contraintes actuelles *)
    let getContradiction e = None
end

