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
  }
  
  (** Construit un graphe (de type t) à partir d'un tableau de litéraux (un noeud par litéral) en y ajoutant un symbole d'absurdité *)
  let make formula deductionCause deductionLevel currentDeductionLevel =
  
    let n = 2 * Formula.getNbVariables formula in
    let matrix = Array.make_matrix (n+1) (n+1) false in
    let node = Array.make (n+1) Invisible in
    let label = Array.make (n+1) "" in
    
    node.(n) <- Red;
    label.(n) <- "Conflict";
    
    let fusion lit l1 l2 =
      let rec accumulate res = function
      | [] -> res
      | a::l when (a = lit || a = Literal.neg lit) -> accumulate res l
      | a::l -> accumulate (a::res) l in
      accumulate (accumulate [] l1) l2 in
    
    let uipFound = ref false in
    let rec explore conflict = 
      List.iter (fun l -> Printf.printf "%d " (Literal.to_int l)) conflict;
      Printf.printf "\n";
      match List.filter (fun l ->
                           let id = Literal.id_of_literal (Literal.neg l) in
                           let level = deductionLevel.(id) in
                           let cause = deductionCause.(id) in
                           match level, cause with
                           | None, _ -> false (* Erreur !! *)
                           | _, None -> false (* C'est un paris *)
                           | Some l, _ -> l = currentDeductionLevel) conflict with
      | [] -> ()
      | a::lst ->
        let litId = Literal.id_of_literal (Literal.neg a) in
        if !uipFound
          then node.(litId) <- Blue
        else if lst <> []
          then node.(litId) <- Purple
          else begin
            uipFound := true;
            node.(litId) <- Yellow;
          end;
        let reasonId = match deductionCause.(litId) with
                       | None -> failwith "This literal is true and it should have a cause."
                       | Some i -> i in
        let reason = Formula.getClause formula reasonId in
        List.iter (fun l -> let id = Literal.id_of_literal (Literal.neg l) in
                            if litId <> Literal.id_of_literal l
                              then matrix.(id).(litId) <- true) reason;
        explore (fusion a conflict reason) in
    
    explore (Formula.getConflict formula);
    
    { adjacencyMatrix = matrix; nodeType = node; nodeLabel = label }

  let export out graph name =
    Printf.fprintf out "digraph %s {\n" name;
    Array.iteri
      (fun id color ->
         let n = Literal.to_int (Literal.literal_of_id id) in
         match color with
         | Invisible -> ()
         | Blue -> Printf.fprintf out "\"%d\" [color=blue]\n" n
         | Red -> Printf.fprintf out "\"%d\" [color=red]\n" n
         | Purple -> Printf.fprintf out "\"%d\" [color=purple]\n" n
         | Yellow -> Printf.fprintf out "\"%d\" [color=yellow]\n" n
      )
      graph.nodeType;
    Array.iteri
      (fun orId line ->
         let ori = Literal.to_int (Literal.literal_of_id orId) in
         Array.iteri (fun destId b ->
             let dest =  Literal.to_int (Literal.literal_of_id destId) in
             if(b) then Printf.fprintf out "\"%d\" -> \"%d\";\n" ori dest;)
           line
      )
      graph.adjacencyMatrix;
    Printf.fprintf out "}\n"
end
