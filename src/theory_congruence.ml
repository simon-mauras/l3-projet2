(** Module implémentant la theorie de la congruence *)

(** Module de type Sigs.Theory_type *)
module Make : Sigs.Theory_type =
  struct
    
    (** Module représentant un terme de la théorie de la congruence *)
    module T =
      struct
      
        type var = string
        type atom = X of var | Fun of var * atom list
        type t = Eq of atom * atom | Neq of atom * atom
        
        let neg = function
          | Eq (a, b) -> Neq (a, b)
          | Neq (a, b) -> Eq (a, b)
        
        let make x =
          let module S = Sigs.Data.Congruence in
          let rec aux_atom = function
          | S.X s -> X s
          | S.Fun (s, l) -> Fun (s, List.map aux_atom l) in
          let aux_term = function
          | S.Eq (a, b) -> Eq (aux_atom a, aux_atom b)
          | S.Neq (a, b) -> Neq (aux_atom a, aux_atom b) in
          let aux_parsing = function
          | Sigs.Parsing_congruence a -> aux_term a
          | _ -> failwith "Wrong parser" in
          let rec aux_formula = function
          | Sigs.And(a, b) -> Sigs.And(aux_formula a, aux_formula b)
          | Sigs.Or(a, b) -> Sigs.Or(aux_formula a, aux_formula b)
          | Sigs.Imp(a, b) -> Sigs.Imp(aux_formula a, aux_formula b)
          | Sigs.Not(a) -> Sigs.Not(aux_formula a)
          | Sigs.Atom(a) -> Sigs.Atom(aux_parsing a) in
          aux_formula x
          
        let compare = Pervasives.compare
        
        let print output x =
          let rec print_atom = function
            | X s -> output_string output s
            | Fun (s, l) -> output_string output s;
                            List.iter (fun u -> output_string output "[";
                                                print_atom u;
                                                output_string output "]") l in
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

