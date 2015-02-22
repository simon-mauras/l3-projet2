(** Module permetant de vérifier que l'entrée est valide *)

(** Affiche sur la sorte founie les messages d'erreur et retourne une formule valide *)
let check out (nb_vars, nb_clauses, clauses) =

    (* Vérifie que les variables sont bien numérotées entre 1 et nb_vars *)
    let max0 x = abs x in
    let rec max1 = function
      | [] -> 0 | x::l -> max (max0 x) (max1 l) in
    let rec max2 = function
      | [] -> 0 | x::l -> max (max1 x) (max2 l) in
    let max_vars = max2 clauses in
    let v = max max_vars nb_vars in
    if max_vars > nb_vars
      then Printf.fprintf out "Warning: variable ids are expected to be between 1 and %d (X%d found).\n" nb_vars v;

    (* Vérifie que le  nombre de clauses est correct *)
    let c = List.length clauses in
    if c <> nb_clauses
      then Printf.fprintf out "Warning: wrong number of clauses (%d found, %d expected).\n" c nb_clauses;
    
    (* Suppression des clauses triviales *)
    let nbTrivial = ref 0 in
    let tabLiterals = Array.make (2*v+1) (-1) in
    let act = ref 0 in
    let cl = List.filter (fun l -> act := !act + 1;
                                   let trivial = ref false in
                                   List.iter (fun i -> if tabLiterals.(v - i) = !act
                                                         then trivial := true;
                                                       tabLiterals.(v + i) <- !act) l;
                                   if !trivial then nbTrivial := !nbTrivial + 1;
                                   not !trivial) clauses in
    if !nbTrivial > 0
      then Printf.fprintf out "Info: %d trivial clause(s) deleted.\n" !nbTrivial;
    
    (v, List.length cl, cl)
