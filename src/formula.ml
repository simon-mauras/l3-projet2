(** Module permettant la manipulation de formules sous forme normale conjonctive *)

open Sigs

(** Module de type Formula_type *)
module Make : Formula_type =
  struct

    module SetL = Set.Make(L)

    (** Etat actuel d'un literal *)
    type literal_state = True | False | Undefined

    (** Information sur les clauses. Le tableau contient pour chaque clause :
        - Le nombre de literaux qui rendent cette clause vrai
        - L'ensemble des literaux dont la valeur est deja fixee
        - L'ensemble des literaux dont la valeur n'est pas encore determinee *)
    type t_clauses = (int * SetL.t * SetL.t) Vector.vector

    (** Information sur les literaux. Le tableau contient pour chaque literal :
        - L'etat actuel du literal
        - Le nombre de clauses non satisfaites dans lequel apparait le literal
        - La liste des clauses dans lesquelles le literal apparait *)
    type t_literals = (literal_state * int * int list) array

    (** Type d'une formule *)
    type t = t_clauses * t_literals

    (** Construit une formule (de type t) a partir d'un element de type cnf *)
    let make out (nb_vars, nb_clauses, clauses) =
      let tabLiterals = Array.make (2*nb_vars) (Undefined, 0, []) in
      let tabClauses = Vector.make nb_clauses (0, SetL.empty, SetL.empty) in
      List.iteri (fun i x -> Vector.set tabClauses i (0, SetL.empty,
                                                      List.fold_left (fun x y ->
                                                          let id = L.id_of_literal y in
                                                          let _,nb,l = tabLiterals.(id) in
                                                          tabLiterals.(id) <- (Undefined, nb+1, i::l);
                                                          SetL.add y x)
                                                        SetL.empty
                                                        (List.map L.make x))) clauses;
      (tabClauses, tabLiterals)

    (** Renvoie le nombre de variables dans la formule *)
    let getNbVariables (_, tabLiterals) = (Array.length tabLiterals) / 2
    
    (** Ajoute une clause a la formule (apprentissage) *)
    let addClause cl formula =
      let (tabClauses, tabLiterals) = formula in
      let n = Vector.length tabClauses in
      Vector.add tabClauses (List.fold_left
        (fun (a, u, v) x ->
           let id = L.id_of_literal x in
           let state,nb,l = tabLiterals.(id) in
           tabLiterals.(id) <- (state, nb+1, n::l);
           match state with
           | Undefined -> (nb, u, SetL.add x v)
           | True -> (a+1, SetL.add x u, v)
           | False -> (a, SetL.add x u, v)) (0, SetL.empty, SetL.empty) cl);
      n (* On retourne l'identifiant de la nouvelle clause *)
    
    (** Affiche la formule et divers informations associees sur la sortie out *)
    let print out formula = 
      let (tabClauses, tabLiterals) = formula in
      Vector.iter (fun (n,u,v)->
          output_string out (string_of_int n);
          SetL.iter (fun x -> output_string out " "; L.print out x) u;
          output_string out " //";
          SetL.iter (fun x -> output_string out " "; L.print out x) v;
          output_string out "\n") tabClauses

    (** Ajoute a la formule l'hypothese que le literal x soit vrai *)
    let setLiteral x formula = 
      let (tabClauses, tabLiterals) = formula in
      let decr z =
        let state, nb, lst = tabLiterals.(L.id_of_literal z) in
        tabLiterals.(L.id_of_literal z) <- (state, nb-1, lst) in
      let _, nb1, lst1 = tabLiterals.(L.id_of_literal x) in
      let _, nb2, lst2 = tabLiterals.(L.id_of_literal (L.neg x)) in
      tabLiterals.(L.id_of_literal x) <- (True, nb1, lst1);
      tabLiterals.(L.id_of_literal (L.neg x)) <- (False, nb2, lst2);
      List.iter (fun i ->
          let n, u, v = Vector.get tabClauses i in
          Vector.set tabClauses i (n+1, SetL.add x u, SetL.remove x v);
          if n = 0
          then SetL.iter decr v
          else decr x) lst1;
      List.iter (fun i ->
          let n, u, v = Vector.get tabClauses i in
          Vector.set tabClauses i (n, SetL.add (L.neg x) u, SetL.remove (L.neg x) v);
          decr (L.neg x)) lst2

    (** Oublie l'hypothese faite sur le literal x dans la formule *)
    let forgetLiteral x formula = 
      let (tabClauses, tabLiterals) = formula in
      let incr z =
        let state, nb, lst = tabLiterals.(L.id_of_literal z) in
        tabLiterals.(L.id_of_literal z) <- (state, nb+1, lst) in
      let _, nb1, lst1 = tabLiterals.(L.id_of_literal x) in
      let _, nb2, lst2 = tabLiterals.(L.id_of_literal (L.neg x)) in
      tabLiterals.(L.id_of_literal x) <- (Undefined, nb1, lst1);
      tabLiterals.(L.id_of_literal (L.neg x)) <- (Undefined, nb2, lst2);
      List.iter (fun i ->
          let n, u, v = Vector.get tabClauses i in
          Vector.set tabClauses i (n-1, SetL.remove x u, SetL.add x v);
          if n = 1 then SetL.iter incr v;
          incr x) lst1;
      List.iter (fun i ->
          let n, u, v = Vector.get tabClauses i in
          Vector.set tabClauses i (n, SetL.remove (L.neg x) u, SetL.add (L.neg x) v);
          incr (L.neg x)) lst2

    (** Renvoie vrai si la formule contient une contradiction triviale sous les hypotheses actuelles *)
    let isFalse formula = 
      let (tabClauses, _) = formula in
      Vector.fold_left (fun b (n,_,u) -> b || (n = 0 && u = SetL.empty)) false tabClauses 
    
    (** Renvoie un literal contenu dans une clause unitaire (sous les hypotheses actuelles) *)
    let getUnitClause (tabClauses,_) =
      let res = ref None in
      for i=0 to (Vector.length tabClauses) - 1 do
        let n,_,v = Vector.get tabClauses i in
        if n = 0 && SetL.cardinal v = 1
          then res := Some (SetL.min_elt v, i)
      done;
      !res

    (** Renvoie la clause d'identifiant i. *)
    let getClause (tabClauses,_) i =
      let _, s1, s2 = Vector.get tabClauses i in
      SetL.fold (fun x l -> x::l) s1 (SetL.fold (fun x l -> x::l) s2 [])
    
    (** Renvoie la clause responsable d'une eventuelle contradiction *)
    let getConflict formula =
      let (tabClauses, _) = formula in
      try Some (getClause formula (Vector.find (fun (n,_,u) -> n = 0 && u = SetL.empty) tabClauses))
      with Not_found -> None
      
  end

