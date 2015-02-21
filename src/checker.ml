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
    if max_vars > nb_vars then begin
      output_string out "Warning: variable ids are expected to be between 1 and ";
      output_string out (string_of_int nb_vars);
      output_string out " (X";
      output_string out (string_of_int max_vars);
      output_string out " found).\n";
    end;

    (* Vérifie que le  nombre de clauses est correct *)
    let c = List.length clauses in
    if c <> nb_clauses then begin
      output_string out "Warning: wrong number of clauses (";
      output_string out (string_of_int c);
      output_string out " found, ";
      output_string out (string_of_int nb_clauses);
      output_string out " expected).\n";
    end;
    
    (v, c, clauses)
