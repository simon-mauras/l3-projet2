(** Module implémentant la theorie de l'égalité *)

(** Module de type Sigs.Theory_type *)
module Make : Sigs.Theory_type =
  struct
    
    (** Module représentant un terme de la théorie de l'égalité *)
    module T =
      struct
        type var = string
        type atom = X of var
        type t = Eq of atom * atom | Neq of atom * atom
        
        let make x =
          let module S = Sigs.Data.Equality in
          match x with
          | Sigs.Parsing_equality (S.Eq (S.X a, S.X b)) -> Eq (X a, X b)
          | Sigs.Parsing_equality (S.Neq (S.X a, S.X b)) -> Eq (X a, X b)
          | _ -> failwith "Wrong parser"
          
        let compare = Pervasives.compare
        
        let print output x =
          let print_atom = function
            | X s -> output_string output s in
          match x with
            | Eq (a, b)  -> print_atom a;
                            output_string output " = ";
                            print_atom b
            | Neq (a, b) -> print_atom a;
                            output_string output " != ";
                            print_atom b
      end
    
    (** Type d'un ensemble de contraintes *)
    type t = T.t option array
    
    (** Un ensemble de contraintes vide *)
    let make tab = tab
    
    (** Ajoute une contrainte *)
    let setConstraint l e = ()
    
    (** Oublie une contrainte *)
    let forgetConstraint l e = ()
    
    (** Renvoie une eventuelle contradiction entre les contraintes actuelles *)
    let getContradiction e = None
end

