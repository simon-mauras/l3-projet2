(** Module permettant la manipulation de formules sous forme normale conjonctive avec le literaux surveilles *)

open Sigs

(** Module de type Formula_type *)
module Make : Formula_type =
  struct

    (** Informations sur les clauses. Le tableau contient pour chaque clause :
        - La liste des literaux de la clause
        - Les iteraux surveilles dans cette clause (si sa taille est >= 2) *)
    type clausesArray = (L.t list * L.t option * L.t option) Vector.vector

    (** Information sur les literaux surveilles.
        Le tableau contient pour chaque literal la liste des clauses dans lesquelles il est surveille *)
    type watchedLiteralsArray = int list array

    (** Informations sur les literaux.
        Le tableau contient pour chaque literal un booleen etant vrai ssi le literal a ete mis a vrai *)
    type literalsArray = bool array

    (** Reponses aux differentes requetes. Cela permet d'eviter un grand nombre de calculs redondants *)
    type requestAnswer = {
      mutable unitClauses : (L.t * int) list;
      mutable conflict : int option
    }

    (** Type d'une formule *)
    type t = clausesArray * watchedLiteralsArray * literalsArray * requestAnswer

    (** Construit une formule (de type t) a partir d'un element de type cnf *)
    let make out (nb_vars, nb_clauses, clauses) =
      let nbOccur = Array.make (2*nb_vars) 0 in
      
      let watchedLiterals = Array.make (2*nb_vars) [] in

      let tabLiterals = Array.make (2*nb_vars) false in

      let answers = {
        unitClauses = [];
        conflict = None
      } in

      let tabClauses = Vector.of_list (List.mapi (fun i l ->
          let lst = List.map (fun a ->
              let x = L.make a in
              nbOccur.(L.id_of_literal x) <- nbOccur.(L.id_of_literal x) + 1;
              x) l in
          match lst with
          | [] ->
            answers.conflict <- Some i;
            (lst, None, None)
          | x::[] ->
            answers.unitClauses <- (x,i)::answers.unitClauses;
            watchedLiterals.(L.id_of_literal x) <- i::watchedLiterals.(L.id_of_literal x);
            (lst, Some x, None)
          | x::y::_->
            watchedLiterals.(L.id_of_literal x) <- i::watchedLiterals.(L.id_of_literal x);
            watchedLiterals.(L.id_of_literal y) <- i::watchedLiterals.(L.id_of_literal y);
            (lst, Some x, Some y)) clauses) in

      (tabClauses, watchedLiterals, tabLiterals, answers);;

    (** Renvoie le nombre de variables dans la formule *)
    let getNbVariables (_,_,literals,_) = (Array.length literals) / 2
    
    (** Ajoute une clause a la formule (apprentissage) *)
    let addClause cl formula =
      let (clauses, watchedLiterals, literals, answers) = formula in
      let id = Vector.length clauses in
      Vector.add clauses (match cl with
                          | [] ->
                            answers.conflict <- Some id;
                            (cl, None, None)
                          | x::[] ->
                            answers.unitClauses <- (x,id)::answers.unitClauses;
                            watchedLiterals.(L.id_of_literal x) <- id::watchedLiterals.(L.id_of_literal x);
                            (cl, Some x, None)
                          | x::y::_->
                            watchedLiterals.(L.id_of_literal x) <- id::watchedLiterals.(L.id_of_literal x);
                            watchedLiterals.(L.id_of_literal y) <- id::watchedLiterals.(L.id_of_literal y);
                            (cl, Some x, Some y));
      id (* On retourne l'identifiant de la nouvelle clause *)

    (** Affiche la formule et divers informations associees sur la sortie out *)
    let print out formula = 
      let (clauses, watchedLiterals, literals, answers) = formula in
      let f (x,_) = L.print out x; output_string out " " in
      output_string out "----------- Clauses ------------\n";
      Vector.iter (function l, a, b ->
          List.iter (fun x ->
              if a = Some x || b = Some x then begin
                output_string out "[";
                L.print out x;
                output_string out "] ";
              end else begin
                L.print out x;
                output_string out " ";
              end) l;
          output_string out "\n") clauses;
      output_string out "--------- unitClauses ----------\n";
      List.iter f answers.unitClauses;
      output_string out "\n----------- isFalse ------------\n";
      if answers.conflict <> None
      then output_string out "Formula is false"
      else output_string out "Can't assert that formula is false";
      output_string out "\n--------------------------------\n"

    (** Ajoute a la formule l'hypothese que le literal x soit vrai *)
    let setLiteral x formula = 
      let (clauses, watchedLiterals, literals, answers) = formula in
      literals.(L.id_of_literal x) <- true;

      let find cl = match cl with
        | _,None,_ -> None
        | _,_,None -> None
        | lst, Some u, Some v ->
          let rec aux = (function
              | [] -> None
              | a::l -> if literals.(L.id_of_literal (L.neg a)) || a = u || a = v
                then aux l
                else Some a)
          in aux lst
      in

      let old = Some (L.neg x) in
      let update i =
        let l, u, v = Vector.get clauses i in
        let other = if old = u then v else u in
        match find (Vector.get clauses i) with
        | Some a ->
          watchedLiterals.(L.id_of_literal a) <- i::watchedLiterals.(L.id_of_literal a);
          Vector.set clauses i (l, Some a, other);
          false (* On change le pointeur, car un autre a ete trouve *)
        | None ->
          (match other with
           | None -> answers.conflict <- Some i
           | Some a ->
             if literals.(L.id_of_literal a)
             then () (* La clause est daje satisfaite *)
             else if literals.(L.id_of_literal (L.neg a))
             then answers.conflict <- Some i (* La clause est fausse *)
             else answers.unitClauses <- (a,i)::answers.unitClauses); (* Il reste une unique maniere pour satisfaire la clause *)
          true (* On garde le pointeur actuel car on en a pas trouve d'autre... *)
      in
      watchedLiterals.(L.id_of_literal (L.neg x)) <-
        List.filter update watchedLiterals.(L.id_of_literal (L.neg x))

    (** Oublie l'hypothese faite sur le literal x dans la formule *)
    let forgetLiteral x formula = 
      let (clauses, watchedLiterals, literals, answers) = formula in
      literals.(L.id_of_literal x) <- false;
      answers.unitClauses <- [];
      answers.conflict <- None

    (** Renvoie vrai si la formule contient une contradiction triviale sous les hypotheses actuelles *)
    let isFalse (_,_,_,answers) = answers.conflict <> None 

    (** Renvoie un literal contenu dans une clause unitaire (sous les hypotheses actuelles) *)
    let rec getUnitClause formula = 
      let (clauses, watchedLiterals, literals, answers) = formula in
      match answers.unitClauses with
      | [] -> None
      | (x,i)::l when (literals.(L.id_of_literal (L.neg x)) || literals.(L.id_of_literal x)) ->
        answers.unitClauses <- l;
        getUnitClause (clauses, watchedLiterals, literals, answers)
      | (x,i)::l -> Some (x,i)
    
    (** Renvoie la clause d'identifiant i. *)
    let getClause formula i =
      let (clauses, watchedLiterals, literals, answers) = formula in
      let cl,_,_ = Vector.get clauses i in cl
    
    (** Renvoie la clause responsable d'une eventuelle contradiction *)
    let getConflict formula =
      let (clauses, watchedLiterals, literals, answers) = formula in
      match answers.conflict with
      | None -> None
      | Some i -> Some (getClause formula i)

  end

