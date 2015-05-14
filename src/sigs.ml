(** Fichier contenant les types et signatures utilisées dans le projet *)

(** Type d'une clause renvoyée par le parser *)
type clause = int list

(** Type d'une formule cnf renvoyée par le parser *)
type cnf = int * int * clause list

(** Type d'un certificat renvoyé par le solver *)
type certificate = int list

(** Type d'une solution pour une formule donnée : il peut exister ou non un certificat *)
type solution = certificate option

module Data = struct
  module Default = struct
    type t = string
  end
  module Equality =
    struct
      type var = string
      type atom = X of var
      type t = Eq of atom * atom | Neq of atom * atom
    end
  module Congruence =
    struct
      type var = string
      type atom = X of var | Fun of var * atom list
      type t = Eq of atom * atom | Neq of atom * atom
    end
  module Difference =
    struct
      type var = string
      type atom = X of var
      type op = Lt | Gt | Leq | Geq | Eq | Neq
      type t =
        | Ternary of atom * atom * op * int (* xi - xj op n *)
        | Binary of atom * op * int (* xi op n *)
    end
end

type parsing =
  | Parsing_default of Data.Default.t
  | Parsing_equality of Data.Equality.t
  | Parsing_congruence of Data.Congruence.t
  | Parsing_difference of Data.Difference.t

(** Type de la représentation d'une formule logique quelconque **)
type 'a formula =
  | And of 'a formula * 'a formula
  | Or of 'a formula * 'a formula
  | Imp of 'a formula * 'a formula
  | Not of 'a formula
  | Atom of 'a

(** Signature d'un module implémentant la manipulation de litéraux *)
module type Literal_type =
sig
  (** Type d'un literal *)
  type t

  (** Construit un litéral (de type t) à partir d'un entier (Format DIMACS CNF) *)
  val make : int -> t

  (** Renvoie un entier représentant le litéral (au format DIMACS CNF) *)
  val to_int : t -> int

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

module Literal : Literal_type = Literal2.Make

module type Theory_type =
  sig
    (** Module représentant un terme de la théorie *)
    module T : sig
      type t
      val make : parsing formula -> t formula
      val compare : t -> t -> int
      val print : out_channel -> t -> unit
    end
    
    (** Type d'un ensemble de contraintes *)
    type t
    
    (** Initialise un ensemble de contraintes vide *)
    val make : T.t option array -> t
    
    (** Ajoute une contrainte *)
    val setConstraint: Literal.t -> t -> unit
    
    (** Oublie une contrainte *)
    val forgetConstraint: Literal.t -> t -> unit
    
    (** Renvoie une eventuelle contradiction entre les contraintes actuelles *)
    val getContradiction: t -> (Literal.t list) option
  end


(** Signature d'un module implémentant la manipulation de formules *)
module type Heuristic_type =
  sig
    (** Type d'une heuristique *)
    type t

    (** Construit une formule (de type t) à partir d'un élément de type cnf *)
    val make: out_channel -> cnf -> t

    (** Ajoute une clause à la formule (apprentissage) *)
    val addClause: Literal.t list -> t -> unit

    (** Affiche divers informations sur une sortie donnée *)
    val print: out_channel -> t -> unit

    (** Ajoute l'hypothèse que le litéral fourni soit vrai à la formule *)
    val setLiteral: Literal.t -> t -> unit

    (** Oublie l'hypothèse faite sur le litéral fourni dans la formule *)
    val forgetLiteral: Literal.t -> t -> unit

    (** Renvoie un litéral sur lequel aucune hypothèse n'a été faite. *)
    val getNextLiteral: t -> Literal.t option 
  end


(** Signature d'un module implémentant la manipulation de formules *)
module type Formula_type =
  sig
    (** Type d'une formule *)
    type t

    (** Construit une formule (de type t) à partir d'un élément de type cnf *)
    val make: out_channel -> cnf -> t

    (** Renvoie le nombre de variables dans la formule *)
    val getNbVariables: t -> int

    (** Ajoute une clause à la formule (apprentissage) *)
    val addClause: Literal.t list -> t -> int

    (** Affiche une formule et divers informations associées sur une sortie donnée *)
    val print: out_channel -> t -> unit

    (** Ajoute l'hypothèse que le litéral fourni soit vrai à la formule *)
    val setLiteral: Literal.t -> t -> unit

    (** Oublie l'hypothèse faite sur le litéral fourni dans la formule *)
    val forgetLiteral: Literal.t -> t -> unit

    (** Renvoie vrai si la formule contient une contradiction triviale sous les hypothèses actuelles *)
    val isFalse: t -> bool

    (** Renvoie le literal et la clause responsable d'une éventuelle contradiction *)
    val getConflict: t -> Literal.t list option

    (** Renvoie un litéral contenu dans une clause unitaire (sous les hypothèses actuelles) *)
    val getUnitClause: t -> (Literal.t * int) option

    (** Renvoie ue clause à partir de son identifiant. *)
    val getClause: t -> int -> Literal.t list
  end

(** Signature d'un module implémentant un algorithme de résolution du problème SAT *)
module type Solver_type =
  functor (H : Heuristic_type) ->
  functor (F : Formula_type) ->
  functor (T : Theory_type) ->
  sig
  
    (** Active l'affichage d'informations de debug *)
    val setDebug: bool -> unit
    
    (** Active l'apprentissage de clause *)
    val setClauseLearning: bool -> unit
    
    (** Active l'apprentissage de clause interactif *)
    val setClauseLearningInteractive: bool -> unit

    (** Renvoie une solution à la formule donnée. Des informations de debug peuvent être afficher sur la sortie donnée *)
    val solve: cnf -> T.T.t option array -> solution
  end

