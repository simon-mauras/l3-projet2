(** Module permettant la manipulation de formules sous forme normale conjonctive avec le litéraux surveillés *)

module L = Sigs.Literal

(** Module de type Sigs.Formla_type *)
module Make : Sigs.Formula_type =
  struct

    (** Informations sur les clauses. Le tableau contient pour chaque clause :
        - La liste des litéraux de la clause
        - Les itéraux surveillés dans cette clause (si sa taille est >= 2) *)
    type clausesArray = (L.t list * L.t option * L.t option) Vector.vector

    (** Information sur les litéraux surveillés.
        Le tableau contient pour chaque literal la liste des clauses dans lesquelles il est surveillé *)
    type watchedLiteralsArray = int list array

    (** Informations sur les litéraux.
        Le tableau contient pour chaque litéral un booléen étant vrai ssi le litéral a été mis à vrai *)
    type literalsArray = bool array

    (** Réponses aux différentes requetes. Cela permet d'éviter un grand nombre de calculs redondants *)
    type requestAnswer = {
      mutable unitClauses : (L.t * int) list;
      mutable pureLiterals : L.t list;
      mutable freeLiterals : L.t list;
      mutable conflict : int option
    }

    (** Type d'une formule *)
    type t = clausesArray * watchedLiteralsArray * literalsArray * requestAnswer

    (** Construit une formule (de type t) à partir d'un élément de type Sigs.cnf *)
    let make out (nb_vars, nb_clauses, clauses) =
      let nbOccur = Array.make (2*nb_vars) 0 in
      let rec literalList = function
        | 0 -> []
        | n -> (L.make n)::(literalList (n-1)) in
      let compare x y =
        let vx = nbOccur.(L.id_of_literal x) + nbOccur.(L.id_of_literal (L.neg x)) in
        let vy = nbOccur.(L.id_of_literal y) + nbOccur.(L.id_of_literal (L.neg y)) in
        vx - vy in
      let watchedLiterals = Array.make (2*nb_vars) [] in

      let tabLiterals = Array.make (2*nb_vars) false in

      let answers = {
        unitClauses = [];
        pureLiterals = [];
        freeLiterals = [];
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

      answers.freeLiterals <- List.sort compare (literalList nb_vars);

      (tabClauses, watchedLiterals, tabLiterals, answers);;

    (** Renvoie le nombre de variables dans la formule *)
    let getNbVariables (_,_,literals,_) = (Array.length literals) / 2
    
    (** Ajoute une clause à la formule (apprentissage) *)
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

    (** Affiche la formule et divers informations associées sur la sortie out *)
    let print out formula = 
      let (clauses, watchedLiterals, literals, answers) = formula in
      let f x = L.print out x; output_string out " " in
      let f2 (x,_) = L.print out x; output_string out " " in
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
      List.iter f2 answers.unitClauses;
      output_string out "\n--------- pureLiterals ---------\n";
      List.iter f answers.pureLiterals;
      output_string out "\n--------- freeLiterals ---------\n";
      List.iter f answers.freeLiterals;
      output_string out "\n----------- isFalse ------------\n";
      if answers.conflict <> None
      then output_string out "Formula is false"
      else output_string out "Can't assert that formula is false";
      output_string out "\n--------------------------------\n"

    (** Ajoute à la formule l'hypothèse que le litéral x soit vrai *)
    let setLiteral x formula = 
      let (clauses, watchedLiterals, literals, answers) = formula in
      literals.(L.id_of_literal x) <- true;

      let rec remove = function
        | [] -> []
        | a::l when a = x -> l
        | a::l when a = L.neg x -> l
        | a::l -> a::(remove l) in
      answers.freeLiterals <- remove answers.freeLiterals;

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
             else answers.unitClauses <- (a,i)::answers.unitClauses); (* Il reste une unique manière pour satisfaire la clause *)
          true (* On garde le pointeur actuel car on en a pas trouve d'autre... *)
      in
      watchedLiterals.(L.id_of_literal (L.neg x)) <-
        List.filter update watchedLiterals.(L.id_of_literal (L.neg x))

    (** Oublie l'hypothèse faite sur le litéral x dans la formule *)
    let forgetLiteral x formula = 
      let (clauses, watchedLiterals, literals, answers) = formula in
      literals.(L.id_of_literal x) <- false;
      answers.unitClauses <- [];
      answers.freeLiterals <- x::answers.freeLiterals;
      answers.conflict <- None

    (** Renvoie vrai si la formule contient une contradiction triviale sous les hypothèses actuelles *)
    let isFalse (_,_,_,answers) = answers.conflict <> None 

    (** Renvoie un litéral contenu dans une clause unitaire (sous les hypothèses actuelles) *)
    let rec getUnitClause formula = 
      let (clauses, watchedLiterals, literals, answers) = formula in
      match answers.unitClauses with
      | [] -> None
      | (x,i)::l when (literals.(L.id_of_literal (L.neg x)) || literals.(L.id_of_literal x)) ->
        answers.unitClauses <- l;
        getUnitClause (clauses, watchedLiterals, literals, answers)
      | (x,i)::l -> Some (x,i)

    (** Renvoie un litéral sur lequel aucune hypothèse n'a été faite. *)
    let getFreeLiteral formula = 
      let (clauses, watchedLiterals, literals, answers) = formula in
      match answers.freeLiterals with
      | [] -> None
      | x::l -> Some x
    
    (** Renvoie la clause d'identifiant i. *)
    let getClause formula i =
      let (clauses, watchedLiterals, literals, answers) = formula in
      let cl,_,_ = Vector.get clauses i in cl
    
    (** Renvoie la clause responsable d'une éventuelle contradiction *)
    let getConflict formula =
      let (clauses, watchedLiterals, literals, answers) = formula in
      match answers.conflict with
      | None -> None
      | Some i -> Some (getClause formula i)

  end

