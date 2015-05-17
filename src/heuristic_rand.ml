(** Module implementant le choix du prochain paris *)

open Sigs

(** Module de type Heuristic_type *)
module Make : Heuristic_type =
  struct
  
  type state = True | False | Undefined
  
  type t = int Vector.vector * int list array * state array
  
  let make (out : out_channel) (nb_vars, nb_clauses, clauses : cnf) : t =
    Random.init 42;
    let tabClauses = Vector.make (nb_clauses) 0 in
    let tabOccurences = Array.make (2*nb_vars) [] in
    let tabStates = Array.make (2*nb_vars) Undefined in
    List.iteri (fun id cl -> List.iter (fun x -> let i = L.id_of_literal (L.make x) in
                                                 tabOccurences.(i) <- id::tabOccurences.(i)) cl) clauses;
    (tabClauses, tabOccurences, tabStates)
  
  let addClause cl (tabClauses, tabOccurences, tabStates : t) =
    let id = Vector.length tabClauses in
    List.iter (fun x -> let i = L.id_of_literal x in
                        tabOccurences.(i) <- id::tabOccurences.(i)) cl;
    let nb = List.fold_left (fun nb x -> if tabStates.(L.id_of_literal x) = True then nb + 1 else nb) 0 cl in
    Vector.add tabClauses nb
    
  let print (out : out_channel) (tabClauses, tabOccurences, tabStates : t) = ()
  
  let setLiteral x (tabClauses, tabOccurences, tabStates : t) =
    tabStates.(L.id_of_literal x) <- True;
    tabStates.(L.id_of_literal (L.neg x)) <- False;
    List.iter (fun i -> Vector.set tabClauses i ((Vector.get tabClauses i) + 1)) tabOccurences.(L.id_of_literal x)
    
  let forgetLiteral x (tabClauses, tabOccurences, tabStates : t) =
    tabStates.(L.id_of_literal x) <- Undefined;
    tabStates.(L.id_of_literal (L.neg x)) <- Undefined;
    List.iter (fun i -> Vector.set tabClauses i ((Vector.get tabClauses i) - 1)) tabOccurences.(L.id_of_literal x)
  
  let getNextLiteral (tabClauses, tabOccurences, tabStates : t) =
    let lst = ref [] in
    Array.iteri (fun i s -> if s = Undefined then lst := i :: !lst) tabStates;
    if !lst = []
      then None
      else
        let id = List.nth !lst (Random.int (List.length !lst)) in
        let l1 = L.literal_of_id id in
        let l2 = L.neg l1 in
        let nb1 = List.fold_left
                    (fun nb i -> if 0 = Vector.get tabClauses i then nb + 1 else nb)
                    0 tabOccurences.(L.id_of_literal l1) in
        let nb2 = List.fold_left
                    (fun nb i -> if 0 = Vector.get tabClauses i then nb + 1 else nb)
                    0 tabOccurences.(L.id_of_literal l2) in
        if (nb1 > nb2) then Some l1 else Some l2
  
  end
