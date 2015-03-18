(** Module implémentant la génération de graphes *)

(** Module de type Sigs.Solver_type *)
module Make (F: Sigs.Formula_type) =
struct

  module Formula = F
  module Literal = F.Literal

  type node = Blue | Yellow | Purple | Red | Invisible
  
  (** Type d'un graphe *)
  type t = {
    adjacencyMatrix : bool array array;
    nodeType: node array;
    nodeLabel: string array;
    learnedClause : Literal.t list;
  }
  
  let sort_uniq cmp l =
    let rec uniq res = function
    | a::b::l when (cmp a b) = 0 -> uniq res (b::l)
    | a::l -> uniq (a::res) l
    | [] -> res in
    uniq [] (List.sort cmp l)
  
  (** Construit un graphe (de type t) à partir d'un tableau de litéraux (un noeud par litéral) en y ajoutant un symbole d'absurdité *)
  let make formula deductionCause deductionLevel currentDeductionLevel =
  
    let n = 2 * Formula.getNbVariables formula in
    let matrix = Array.make_matrix (n+1) (n+1) false in
    let node = Array.make (n+1) Invisible in
    let label = Array.init (n+1) (fun i -> string_of_int (Literal.to_int (Literal.literal_of_id i))) in
    let clause = ref [] in
    
    node.(n) <- Red;
    label.(n) <- "Conflict";
    
    let fusion lit l1 l2 =
      let rec accumulate res = function
      | [] -> res
      | a::l when (a = lit || a = Literal.neg lit) -> accumulate res l
      | a::l -> accumulate (a::res) l in
      accumulate (accumulate [] l1) l2 in
    
    let uipFound = ref false in
    let rec explore cl =
      let conflict = sort_uniq (Literal.compare) cl in
      (*List.iter (fun l -> Printf.printf "%d " (Literal.to_int l)) conflict;
      Printf.printf "\n";*)
      let level = List.filter (fun lit ->
                                match deductionLevel.(Literal.id_of_literal (Literal.neg lit)) with
                                | None -> false (* Erreur *)
                                | Some l -> l = currentDeductionLevel) conflict in
                                
      let cause = List.filter (fun l -> deductionCause.(Literal.id_of_literal (Literal.neg l)) <> None) level in
      
      (*List.iter (fun l -> Printf.printf "%d " (Literal.to_int l)) level;
      Printf.printf "\n";
      List.iter (fun l -> Printf.printf "%d " (Literal.to_int l)) cause;
      Printf.printf "\n";*)
      
      match level, cause, !uipFound with
      | a::[],_,false -> uipFound := true;
                         node.(Literal.id_of_literal (Literal.neg a)) <- Yellow;
                         clause := conflict;
      | _,[],_ -> ()
      | _,a::l,true  -> node.(Literal.id_of_literal (Literal.neg a)) <- Blue;
      | _,a::l,false -> node.(Literal.id_of_literal (Literal.neg a)) <- Purple;
                     
      match cause with
      | [] -> ()
      | a::lst ->
        let litId = Literal.id_of_literal (Literal.neg a) in
        let reasonId = match deductionCause.(litId) with
                       | None -> failwith "This literal is true and it should have a cause."
                       | Some i -> i in
        let reason = Formula.getClause formula reasonId in
        List.iter (fun l -> let id = Literal.id_of_literal (Literal.neg l) in
                            if litId <> Literal.id_of_literal l
                              then matrix.(id).(litId) <- true) reason;
        (*List.iter (fun l -> Printf.printf "%d " (Literal.to_int l)) reason;
        Printf.printf "\n";
        Printf.printf "\n";*)
        explore (fusion a conflict reason) in
    
    let conflict = Formula.getConflict formula in
    List.iter (fun l -> let id = Literal.id_of_literal (Literal.neg l) in
                        matrix.(id).(n) <- true) conflict;
    explore conflict;
    
    { adjacencyMatrix = matrix; nodeType = node; nodeLabel = label; learnedClause = !clause }

  let getLearnedClause graph = graph.learnedClause
  
  let export out graph name =
    Printf.fprintf out "digraph %s {\n" name;
    Array.iteri
      (fun id color ->
         let s = graph.nodeLabel.(id) in
         match color with
         | Invisible -> ()
         | Blue -> Printf.fprintf out "\"%s\" [style=filled,color=blue]\n" s
         | Red -> Printf.fprintf out "\"%s\" [style=filled,color=red]\n" s
         | Purple -> Printf.fprintf out "\"%s\" [style=filled,color=purple]\n" s
         | Yellow -> Printf.fprintf out "\"%s\" [style=filled,color=yellow]\n" s
      )
      graph.nodeType;
    Array.iteri (fun orId line ->
                   Array.iteri (fun destId b ->
                                  if b then Printf.fprintf out "\"%s\" -> \"%s\";\n" graph.nodeLabel.(orId) graph.nodeLabel.(destId))
                               line)
                 graph.adjacencyMatrix;
    Printf.fprintf out "}\n"
end
