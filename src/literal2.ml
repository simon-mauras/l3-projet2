(** Module permettant de manipuler les litéraux *)

(** Module de type Literal_type *)
module Make: Sigs.Literal_type =
  struct
  
    (** Type d'un literal *)
    type t = int
    
    (** Construit un litéral (de type t) à partir d'un entier (Format DIMACS CNF) *)
    let make x = x
    
    (** Renvoie un entier représentant le litéral (au format DIMACS CNF) *)
    let to_int x = x
    
    (** Affiche un litéral sur la sortie out *)
    let print out x =
      if x > 0 
        then Printf.fprintf out "X(%d)" x
        else Printf.fprintf out "Xbar(%d)" (-x)
      
    (** Fonction de comparaison entre deux litéraux (utile pour construire un Set de litéraux) *)
    let compare a b = a - b
      
    (** Renvoi la négation d'un litéral *)
    let neg x = -x
      
    (** Renvoie un identifiant associé au litéral (entier entre 0 et 2*nbVars+1 si les variables sont numérotées entre 1 et nbVars) *)
    let id_of_literal x =
      if x > 0 
        then 2*(x-1)
        else 2*(-x-1) + 1
    
    (** Renvoie le litéral associé à un identifiant donné *)
    let literal_of_id n =
      if n mod 2 = 0
        then (1+n/2)
        else (-1-n/2)
  end
