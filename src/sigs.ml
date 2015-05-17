(** Fichier contenant les types et signatures utilisees dans le projet *)

(** Type d'une clause renvoyee par le parser *)
type clause = int list

(** Type d'une formule cnf renvoyee par le parser *)
type cnf = int * int * clause list

(** Type d'un certificat renvoye par le solver *)
type certificate = int list

(** Type d'une solution pour une formule donnee : il peut exister ou non un certificat *)
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

(** Type de la representation d'une formule logique quelconque *)
type 'a formula =
  | And of 'a formula * 'a formula
  | Or of 'a formula * 'a formula
  | Imp of 'a formula * 'a formula
  | Not of 'a formula
  | Atom of 'a

(** Signature d'un module implementant la manipulation de literaux *)
module type Literal_type =
sig
  (** Type d'un literal *)
  type t

  (** Construit un literal (de type t) a partir d'un entier (Format DIMACS CNF) *)
  val make : int -> t

  (** Renvoie un entier representant le literal (au format DIMACS CNF) *)
  val to_int : t -> int

  (** Affiche un literal sur une sortie donnee *)
  val print : out_channel -> t -> unit

  (** Fonction de comparaison entre deux literaux (utile pour construire un Set de literaux) *)
  val compare : t -> t -> int

  (** Renvoi la negation d'un literal *)
  val neg : t -> t

  (** Renvoie un identifiant associe au literal (entier entre 0 et 2*nbVars+1 si les variables sont numerotees entre 1 et nbVars) *)
  val id_of_literal : t -> int

  (** Renvoie le literal associe a un identifiant donne *)
  val literal_of_id : int -> t
end

(** Module par defaut pour la manipulation de literaux *)
module L : Literal_type = Literal2.Make

(** Signature d'un module implementant une theorie *)
module type Theory_type =
  sig
    (** Module representant un terme de la theorie *)
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
    val setConstraint: L.t -> t -> unit
    
    (** Oublie une contrainte *)
    val forgetConstraint: L.t -> t -> unit
    
    (** Renvoie une eventuelle contradiction entre les contraintes actuelles *)
    val getContradiction: t -> (L.t list) option
  end


(** Signature d'un module implementant l'heuristique pour le choix du prochain paris *)
module type Heuristic_type =
  sig
    (** Type d'une heuristique *)
    type t

    (** Construit une formule (de type t) a partir d'un element de type cnf *)
    val make: out_channel -> cnf -> t

    (** Ajoute une clause a la formule (apprentissage) *)
    val addClause: L.t list -> t -> unit

    (** Affiche divers informations sur une sortie donnee *)
    val print: out_channel -> t -> unit

    (** Ajoute l'hypothese que le literal fourni soit vrai a la formule *)
    val setLiteral: L.t -> t -> unit

    (** Oublie l'hypothese faite sur le literal fourni dans la formule *)
    val forgetLiteral: L.t -> t -> unit

    (** Renvoie un literal sur lequel aucune hypothese n'a ete faite. *)
    val getNextLiteral: t -> L.t option 
  end


(** Signature d'un module implementant la manipulation de formules *)
module type Formula_type =
  sig
    (** Type d'une formule *)
    type t

    (** Construit une formule (de type t) a partir d'un element de type cnf *)
    val make: out_channel -> cnf -> t

    (** Renvoie le nombre de variables dans la formule *)
    val getNbVariables: t -> int

    (** Ajoute une clause a la formule (apprentissage) *)
    val addClause: L.t list -> t -> int

    (** Affiche une formule et divers informations associees sur une sortie donnee *)
    val print: out_channel -> t -> unit

    (** Ajoute l'hypothese que le literal fourni soit vrai a la formule *)
    val setLiteral: L.t -> t -> unit

    (** Oublie l'hypothese faite sur le literal fourni dans la formule *)
    val forgetLiteral: L.t -> t -> unit

    (** Renvoie vrai si la formule contient une contradiction triviale sous les hypotheses actuelles *)
    val isFalse: t -> bool

    (** Renvoie le literal et la clause responsable d'une eventuelle contradiction *)
    val getConflict: t -> L.t list option

    (** Renvoie un literal contenu dans une clause unitaire (sous les hypotheses actuelles) *)
    val getUnitClause: t -> (L.t * int) option

    (** Renvoie ue clause a partir de son identifiant. *)
    val getClause: t -> int -> L.t list
  end

(** Signature d'un module implementant un algorithme de resolution du probleme SAT *)
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

    (** Renvoie une solution a la formule donnee. Des informations de debug peuvent être afficher sur la sortie donnee *)
    val solve: cnf -> T.T.t option array -> solution
  end

