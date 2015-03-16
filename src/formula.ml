(** Module permettant la manipulation de formules sous forme normale conjonctive *)

(** Module de type Sigs.Formla_type *)
module Make (L: Sigs.Literal_type) : Sigs.Formula_type =
struct

  module Literal = L
  module LiteralSet = Set.Make(Literal)

  (** Etat actuel d'un literal *)
  type literal_state = True | False | Undefined

  (** Information sur les clauses. Le tableau contient pour chaque clause :
      - Le nombre de litéraux qui rendent cette clause vrai
      - L'ensemble des litéraux dont la valeur est déjà fixée
      - L'ensemble des litéraux dont la valeur n'est pas encore déterminée *)
  type t_clauses = (int * LiteralSet.t * LiteralSet.t) Vector.vector

  (** Information sur les litéraux. Le tableau contient pour chaque litéral :
      - L'état actuel du litéral
      - Le nombre de clauses non satisfaites dans lequel apparait le litéral
      - La liste des clauses dans lesquelles le litéral apparait *)
  type t_literals = (literal_state * int * int list) array

  (** Type d'une formule *)
  type t = t_clauses * t_literals

  (** Construit une formule (de type t) à partir d'un élément de type Sigs.cnf *)
  let make out (nb_vars, nb_clauses, clauses) =
    let tabLiterals = Array.make (2*nb_vars) (Undefined, 0, []) in
    let tabClauses = Vector.make nb_clauses (0, LiteralSet.empty, LiteralSet.empty) in
    List.iteri (fun i x -> Vector.set tabClauses i (0, LiteralSet.empty,
                                                    List.fold_left (fun x y ->
                                                        let id = Literal.id_of_literal y in
                                                        let _,nb,l = tabLiterals.(id) in
                                                        tabLiterals.(id) <- (Undefined, nb+1, i::l);
                                                        LiteralSet.add y x)
                                                      LiteralSet.empty
                                                      (List.map Literal.make x))) clauses;
    (tabClauses, tabLiterals)

  (** Ajoute une clause à la formule (apprentissage) *)
  let addClause cl formula = ()
  
  (** Affiche la formule et divers informations associées sur la sortie out *)
  let print out formula = 
    let (tabClauses, tabLiterals) = formula in
    Vector.iter (fun (n,u,v)->
        output_string out (string_of_int n);
        LiteralSet.iter (fun x -> output_string out " "; Literal.print out x) u;
        output_string out " //";
        LiteralSet.iter (fun x -> output_string out " "; Literal.print out x) v;
        output_string out "\n") tabClauses

  (** Ajoute à la formule l'hypothèse que le litéral x soit vrai *)
  let setLiteral x formula = 
    let (tabClauses, tabLiterals) = formula in
    let decr z =
      let state, nb, lst = tabLiterals.(Literal.id_of_literal z) in
      tabLiterals.(Literal.id_of_literal z) <- (state, nb-1, lst) in
    let _, nb1, lst1 = tabLiterals.(Literal.id_of_literal x) in
    let _, nb2, lst2 = tabLiterals.(Literal.id_of_literal (Literal.neg x)) in
    tabLiterals.(Literal.id_of_literal x) <- (True, nb1, lst1);
    tabLiterals.(Literal.id_of_literal (Literal.neg x)) <- (False, nb2, lst2);
    List.iter (fun i ->
        let n, u, v = Vector.get tabClauses i in
        Vector.set tabClauses i (n+1, LiteralSet.add x u, LiteralSet.remove x v);
        if n = 0
        then LiteralSet.iter decr v
        else decr x) lst1;
    List.iter (fun i ->
        let n, u, v = Vector.get tabClauses i in
        Vector.set tabClauses i (n, LiteralSet.add (Literal.neg x) u, LiteralSet.remove (Literal.neg x) v);
        decr (Literal.neg x)) lst2

  (** Oublie l'hypothèse faite sur le litéral x dans la formule *)
  let forgetLiteral x formula = 
    let (tabClauses, tabLiterals) = formula in
    let incr z =
      let state, nb, lst = tabLiterals.(Literal.id_of_literal z) in
      tabLiterals.(Literal.id_of_literal z) <- (state, nb+1, lst) in
    let _, nb1, lst1 = tabLiterals.(Literal.id_of_literal x) in
    let _, nb2, lst2 = tabLiterals.(Literal.id_of_literal (Literal.neg x)) in
    tabLiterals.(Literal.id_of_literal x) <- (Undefined, nb1, lst1);
    tabLiterals.(Literal.id_of_literal (Literal.neg x)) <- (Undefined, nb2, lst2);
    List.iter (fun i ->
        let n, u, v = Vector.get tabClauses i in
        Vector.set tabClauses i (n-1, LiteralSet.remove x u, LiteralSet.add x v);
        if n = 1 then LiteralSet.iter incr v;
        incr x) lst1;
    List.iter (fun i ->
        let n, u, v = Vector.get tabClauses i in
        Vector.set tabClauses i (n, LiteralSet.remove (Literal.neg x) u, LiteralSet.add (Literal.neg x) v);
        incr (Literal.neg x)) lst2

  (** Renvoie vrai si la formule contient une contradiction triviale sous les hypothèses actuelles *)
  let isFalse formula = 
    let (tabClauses, _) = formula in
    Vector.fold_left (fun b (n,_,u) -> b || (n = 0 && u = LiteralSet.empty)) false tabClauses 

  (** Renvoie un litéral contenu dans une clause unitaire (sous les hypothèses actuelles) *)
  let getUnitClause (tabClauses,_) =
    let res = ref None in
    for i=0 to (Vector.length tabClauses) - 1 do
      let n,_,v = Vector.get tabClauses i in
      if n = 0 && LiteralSet.cardinal v = 1
        then res := Some (LiteralSet.min_elt v, i)
    done;
    !res
  
  (*
  (** Renvoie un litéral dont la négation est absente de la formule (sous les hypothèses actuelles) *)
  let getPureLiteral formula = 
    let (_, tabLiterals) = formula in
    let ind = ref (-1) in
    for i = 0 to (Array.length tabLiterals) - 1 do
      let state, n, lst = tabLiterals.(i) in
      if (state, n) = (Undefined, 0)
      then ind := i;
    done;
    if !ind <> -1
    then Some (Literal.neg (Literal.literal_of_id !ind))
    else None
  *)
  
  (** Renvoie un litéral sur lequel aucune hypothèse n'a été faite. *)
  let getFreeLiteral (_, tabLiterals) =
    let value = ref min_int in
    let index = ref 0 in
    for i = 0 to (Array.length tabLiterals) - 1 do
      let state,v,_ = tabLiterals.(i) in
      if v > !value && state = Undefined then begin
        value := v;
        index := i;
      end;
    done;
    if !value <> min_int
    then Some(Literal.literal_of_id !index)
    else None

  (** Renvoie la clause d'identifiant i. *)
  let getClause (tabClauses,_) i =
    let _, s1, s2 = Vector.get tabClauses i in
    LiteralSet.fold (fun x l -> x::l) s1 (LiteralSet.fold (fun x l -> x::l) s2 [])
  
end

