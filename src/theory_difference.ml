(** Module implémentant la theorie de la congruence *)

(** Module de type Sigs.Theory_type *)
module Make : Sigs.Theory_type =
  struct
    
    (** Module représentant un terme de la théorie de la congruence *)
    module T =
      struct
        type var = string
        type atom = X of var | Zero
        type t = atom * atom * int (* xi - xj <= n *)
        
        let make x =
          let module S = Sigs.Data.Difference in
          let rec aux_atom = function
          | S.X s -> X s in
          let aux_term = function
          | S.Ternary (a, b, S.Lt, n)  -> Sigs.Atom (aux_atom a, aux_atom b, n-1)
          | S.Ternary (a, b, S.Leq, n) -> Sigs.Atom (aux_atom a, aux_atom b, n)
          | S.Ternary (a, b, S.Gt, n)  -> Sigs.Atom (aux_atom b, aux_atom a, 1-n)
          | S.Ternary (a, b, S.Geq, n) -> Sigs.Atom (aux_atom b, aux_atom a, -n)
          | S.Ternary (a, b, S.Eq, n)  -> Sigs.And (Sigs.Atom (aux_atom a, aux_atom b, n),   Sigs.Atom (aux_atom b, aux_atom a, -n))
          | S.Ternary (a, b, S.Neq, n) -> Sigs.Or  (Sigs.Atom (aux_atom a, aux_atom b, n-1), Sigs.Atom (aux_atom b, aux_atom a, 1-n))
          | S.Binary (a, S.Lt, n)  -> Sigs.Atom (aux_atom a, Zero, n-1)
          | S.Binary (a, S.Leq, n) -> Sigs.Atom (aux_atom a, Zero, n)
          | S.Binary (a, S.Gt, n)  -> Sigs.Atom (Zero, aux_atom a, 1-n)
          | S.Binary (a, S.Geq, n) -> Sigs.Atom (Zero, aux_atom a, -n)
          | S.Binary (a, S.Eq, n)  -> Sigs.And (Sigs.Atom (aux_atom a, Zero, n),   Sigs.Atom (Zero, aux_atom a, -n))
          | S.Binary (a, S.Neq, n) -> Sigs.Or  (Sigs.Atom (aux_atom a, Zero, n-1), Sigs.Atom (Zero, aux_atom a, 1-n)) in
          let aux_parsing = function
          | Sigs.Parsing_difference a -> aux_term a
          | _ -> failwith "Wrong parser" in
          let rec aux_formula = function
          | Sigs.And(a, b) -> Sigs.And(aux_formula a, aux_formula b)
          | Sigs.Or(a, b) -> Sigs.Or(aux_formula a, aux_formula b)
          | Sigs.Imp(a, b) -> Sigs.Imp(aux_formula a, aux_formula b)
          | Sigs.Not(a) -> Sigs.Not(aux_formula a)
          | Sigs.Atom(a) -> aux_parsing a in
          aux_formula x
          
        let compare = Pervasives.compare
        
        let print output (a, b, n) =
          let print_atom = function
            | X s -> output_string output s
            | Zero -> output_string output "Zero" in
          print_atom a;
          output_string output " - ";
          print_atom b;
          output_string output " <= ";
          output_string output (string_of_int n)
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

