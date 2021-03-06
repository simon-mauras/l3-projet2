(** Module implementant le choix du prochain paris *)

open Sigs

(** Module de type Heuristic_type *)
module Make : Heuristic_type =
  struct
  
  type state = True | False | Undefined
  
  type t = float array * state array * int ref
  
  let make (out : out_channel) (nb_vars, nb_clauses, clauses : cnf) : t =
    let tabScores = Array.make (2*nb_vars) 0. in
    let tabStates = Array.make (2*nb_vars) Undefined in
    (tabScores, tabStates, ref 0)
  
  let addClause cl (tabScores, tabStates, count : t) =
    List.iter (fun x -> let i = L.id_of_literal x in
                        tabScores.(i) <- 1. +. tabScores.(i)) cl
    
  let print (out : out_channel) (tabScores, tabStates, count : t) = ()
  
  let setLiteral x (tabScores, tabStates, count : t) =
    tabStates.(L.id_of_literal x) <- True;
    tabStates.(L.id_of_literal (L.neg x)) <- False
    
  let forgetLiteral x (tabScores, tabStates, count : t) =
    tabStates.(L.id_of_literal x) <- Undefined;
    tabStates.(L.id_of_literal (L.neg x)) <- Undefined
  
  let getNextLiteral (tabScores, tabStates, count : t) =
    decr count;
    if !count <= 0 then begin
      count := 20;
      Array.iteri (fun i v -> tabScores.(i) <- v /. 2.) tabScores;
    end;
    let lst = ref [] in
    Array.iteri (fun i s -> if s = Undefined then lst := i :: !lst) tabStates;
    if !lst = []
      then None
      else Some (L.literal_of_id (List.fold_left
                                    (fun best act ->
                                       if tabScores.(act) > tabScores.(best)
                                         then act else best)
                                    (List.hd !lst) !lst))
  
  end
