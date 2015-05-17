(** Module permettant de manipuler les literaux *)

(** Module de type Literal_type *)
module Make =
struct

  (** Type d'un literal *)
  type t = X of int | Xbar of int

  (** Construit un literal (de type t) a partir d'un entier (Format DIMACS CNF) *)
  let make x = if x < 0 then Xbar(-x) else X(x)

  (** Renvoie un entier representant le literal (au format DIMACS CNF) *)
  let to_int = function
    | X x -> x
    | Xbar x -> -x

  (** Affiche un literal sur la sortie out *)
  let print out = function
    | X x    -> output_string out "X(";    output_string out (string_of_int x); output_string out ")"
    | Xbar x -> output_string out "Xbar("; output_string out (string_of_int x); output_string out ")"

  (** Fonction de comparaison entre deux literaux (utile pour construire un Set de literaux) *)
  let compare a b = match a, b with
    | X xa, Xbar xb    when xa = xb -> 1
    | Xbar xa, X xb    when xa = xb -> -1
    | X xa, X xb       when xa = xb -> 0
    | Xbar xa, Xbar xb when xa = xb -> 0
    | X xa, Xbar xb | Xbar xa, X xb | X xa, X xb | Xbar xa, Xbar xb -> if xa < xb then 1 else -1

  (** Renvoi la negation d'un literal *)
  let neg = function
    | X x -> Xbar x
    | Xbar x -> X x

  (** Renvoie un identifiant associe au literal (entier entre 0 et 2*nbVars+1 si les variables sont numerotees entre 1 et nbVars) *)
  let id_of_literal = function
    | X x -> 2*(x-1)
    | Xbar x -> 2*(x-1) + 1

  (** Renvoie le literal associe a un identifiant donne *)
  let literal_of_id n =
    if n mod 2 = 0
    then X (1+n/2)
    else Xbar (1+n/2)
end
