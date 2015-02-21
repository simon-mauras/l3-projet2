(** Fichier contenant les types et signatures utilisées dans le projet *)

(** Type d'une formule cnf renvoyée par le parser *)
type cnf = int * int * int list list
(** Type d'un certificat renvoyé par le solver *)
type certificate = int list
(** Type d'une solution pour une formule donnée : il peut exister ou non un certificat *)
type solution = certificate option

(** Signature d'un module implémentant la manipulation de litéraux *)
module type Literal_type =
sig
  (** Type d'un literal *)
  type t = X of int | Xbar of int
  (** Construit un litéral (de type t) à partir d'un entier (Format DIMACS CNF) *)
  val make : int -> t
  (** Affiche un litéral sur une sortie donnée *)
  val print : out_channel -> t -> unit
  (** Fonction de comparaison entre deux litéraux (utile pour construire un Set de litéraux) *)
  val compare : t -> t -> int
  (** Renvoi la négation d'un litéral *)
  val neg : t -> t
  (** Renvoie un identifiant associé au litéral (entier entre 0 et 2*nbVars+1 si les variables sont numérotées entre 1 et nbVars) *)
  val id_of_literal : t -> int
  (** Renvoie le litéral associé à un identifiant donné *)
  val literal_of_id : int -> t
end

(** Signature d'un module implémentant la manipulation de formules *)
module type Formula_type =
sig
  (** Type d'une formule *)
  type t
  (** Module implémentant la manipulation de litéraux *)
  module Literal : Literal_type

  (** Construit une formule (de type t) à partir d'un élément de type cnf *)
  val make: out_channel -> cnf -> t
  (** Affiche une formule et divers informations associées sur une sortie donnée *)
  val print: out_channel -> t -> unit

  (** Ajoute l'hypothèse que le litéral fourni soit vrai à la formule *)
  val setLiteral: Literal.t -> t -> unit
  (** Oublie l'hypothèse faite sur le litéral fourni dans la formule *)
  val forgetLiteral: Literal.t -> t -> unit

  (** Renvoie vrai si la formule contient une contradiction triviale sous les hypothèses actuelles *)
  val isFalse: t -> bool
  (** Renvoie un litéral contenu dans une clause unitaire (sous les hypothèses actuelles) *)
  val getUnitClause: t -> Literal.t option
  (** Renvoie un litéral dont la négation est absente de la formule (sous les hypothèses actuelles) *)
  val getPureLiteral: t -> Literal.t option
  (** Renvoie un litéral sur lequel aucune hypothèse n'a été faite. *)
  val getFreeLiteral: t -> Literal.t option 
end

(** Signature d'un module implémentant un algorithme de résolution du problème SAT *)
module type Solver_type =
  functor (F : Formula_type) ->
  sig
    (** Renvoie une solution à la formule donnée. Des informations de debug peuvent être afficher sur la sortie donnée *)
    val solve: out_channel -> F.t -> solution
  end

