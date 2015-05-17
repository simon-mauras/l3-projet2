(** Module implementant le choix du prochain paris *)

open Sigs

(** Module de type Heuristic_type *)
module Make : Heuristic_type =
  struct
  
  type state = True | False | Undefined
  
  type t = L.t list list ref * state array
  
  let make (out : out_channel) (nb_vars, nb_clauses, clauses : cnf) : t =
    let tabStates = Array.make (2*nb_vars) Undefined in
    let lstClauses = List.map (List.map L.make) clauses in
    (ref lstClauses, tabStates)
  
  let addClause cl (lstClauses, tabStates : t) =
    lstClauses := cl :: !lstClauses
    
  let print (out : out_channel) (lstClauses, tabStates : t) = ()
  
  let setLiteral x (lstClauses, tabStates : t) =
    tabStates.(L.id_of_literal x) <- True;
    tabStates.(L.id_of_literal (L.neg x)) <- False
    
  let forgetLiteral x (lstClauses, tabStates : t) =
    tabStates.(L.id_of_literal x) <- Undefined;
    tabStates.(L.id_of_literal (L.neg x)) <- Undefined
  
  let getNextLiteral (lstClauses, tabStates : t) =
    let taille = ref max_int in
    let lst = ref [] in
    List.iter (fun cl ->
               let act = List.fold_left
                           (fun nb x -> match tabStates.(L.id_of_literal x) with
                              | Undefined -> nb+1
                              | True -> max_int / 2
                              | False -> nb) 0 cl in
               if act < !taille then lst := [];
               taille := min !taille act;
               if act = !taille then lst := cl :: !lst) !lstClauses;
    let tabOccurences = Array.make (Array.length tabStates) 0 in
    List.iter (List.iter (fun x ->
                          if tabStates.(L.id_of_literal x) = Undefined
                            then tabOccurences.(L.id_of_literal x) <- tabOccurences.(L.id_of_literal x)+1)) !lst;
    let res = ref 0 in
    Array.iteri (fun i x -> if x = Undefined then res := i) tabStates;
    Array.iteri (fun i x -> if x > tabOccurences.(!res) then res := i) tabOccurences;
    if tabStates.(!res) <> Undefined
      then None
      else Some (L.literal_of_id !res)
  
  end
