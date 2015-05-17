(** Module permettant de manipuler les literaux *)

(** Module de type Literal_type *)
module Make =
struct

  (** Type d'un literal *)
  type t = int

  (** Construit un literal (de type t) a partir d'un entier (Format DIMACS CNF) *)
  let make (x : int) : t = x

  (** Renvoie un entier representant le literal (au format DIMACS CNF) *)
  let to_int (x : t) : int = x

  (** Affiche un literal sur la sortie out *)
  let print out (x : t) =
    if x > 0 
    then Printf.fprintf out "X(%d)" x
    else Printf.fprintf out "Xbar(%d)" (-x)

  (** Fonction de comparaison entre deux literaux (utile pour construire un Set de literaux) *)
  let compare (a : t) (b : t) : int = a - b

  (** Renvoi la negation d'un literal *)
  let neg (x : t) : t = -x

  (** Renvoie un identifiant associe au literal (entier entre 0 et 2*nbVars-1 si les variables sont numerotees entre 1 et nbVars) *)
  let id_of_literal (x : t) : int =
    if x > 0 
    then 2*(x-1)
    else 2*(-x-1) + 1

  (** Renvoie le literal associe a un identifiant donne *)
  let literal_of_id (n : int) : t =
    if n mod 2 = 0
    then (1+n/2)
    else (-1-n/2)
end
