(** Module implémentant la génération de graphes *)

module L = Sigs.Literal
  
(** Module de type Sigs.Solver_type *)
module Make (F: Sigs.Formula_type) =
struct

  type node = Blue | Yellow | Purple | Red | White | Invisible
  
  (** Type d'un graphe *)
  type t = {
    adjacencyMatrix : bool array array;
    nodeType: node array;
    nodeLabel: string array;
    learnedClause : L.t list;
    uip : L.t;
  }
  
  (** Construit un graphe (de type t) à partir d'un tableau de litéraux (un noeud par litéral) en y ajoutant un symbole d'absurdité *)
  let make conflict formula deductionCause deductionLevel currentDeductionLevel =
  
    let n = 2 * F.getNbVariables formula in
    let matrix = Array.make_matrix (n+1) (n+1) false in
    let node = Array.make (n+1) Invisible in
    let label = Array.init (n+1) (fun i -> string_of_int (L.to_int (L.literal_of_id i))) in
    
    node.(n) <- Red;
    label.(n) <- "Conflict";
    
    let topSort = ref [] in
    let rec explore i =
      if node.(i) = Invisible then
         match deductionLevel.(i) with
         | None -> failwith (string_of_int (L.to_int (L.literal_of_id i)))
         | Some level when level <> currentDeductionLevel ->
           node.(i) <- White
         | Some level ->
           node.(i) <- Blue;
          (match deductionCause.(i) with
           | None -> () (* Il s'agit d'un paris *)
           | Some idReason ->
             let reason = F.getClause formula idReason in
             List.iter (fun l -> let id = L.id_of_literal (L.neg l) in
                                 if i <> L.id_of_literal l then begin
                                   explore id;
                                   matrix.(id).(i) <- true;
                                 end) reason);
           topSort := i::!topSort;
      in
    
    List.iter (fun l -> let id = L.id_of_literal (L.neg l) in
                        explore id;
                        matrix.(id).(n) <- true) conflict;
                    
    let nbWaiting = ref 0 in
    let waiting = Array.make n false in
    let clause = ref [] in
    let uip = ref None in
    
    let propagation i =
      if !uip = None then begin
        decr nbWaiting;
        if !nbWaiting = 0
          then uip := Some (L.literal_of_id i)
          else match deductionCause.(i) with
               | None -> () (* Il s'agit d'un paris *)
               | Some idReason ->
                 let reason = F.getClause formula idReason in
                 List.iter (fun l -> let id = L.id_of_literal (L.neg l) in
                                     if not waiting.(id) then begin
                                       waiting.(id) <- true;
                                       if node.(id) = Blue
                                         then incr nbWaiting
                                       else if node.(id) = White
                                         then clause := l::!clause
                                     end) reason;
      end in
    
    List.iter (fun l -> let id = L.id_of_literal (L.neg l) in
                        if not waiting.(id) then begin
                          waiting.(id) <- true;
                          if node.(id) = Blue
                             then incr nbWaiting
                          else if node.(id) = White
                            then clause := l::!clause
                        end) conflict;
    
    List.iter propagation !topSort;
    
    let u = match !uip with
            | None -> failwith "Uip not found"
            | Some i -> i in
    
    clause := (L.neg u)::!clause;
    
    let rec dfs act =
      node.(act) <- Purple;
      for next=0 to n-1 do
        if node.(next) <> Purple && matrix.(act).(next)
          then dfs next
      done in
    
    dfs (L.id_of_literal u);
    node.(L.id_of_literal u) <- Yellow;
    
    { adjacencyMatrix = matrix; nodeType = node; nodeLabel = label; learnedClause = !clause; uip = u }

  (** Renvoie la clause apprise *)
  let getLearnedClause graph = graph.learnedClause
  
  (** Renvoie le premier UIP du graphe *)
  let getUip graph = graph.uip
  
  (** Exporte un graphe (format dot) dans la sortie out *)
  let export out graph name =
    Printf.fprintf out "digraph %s {\n" name;
    Array.iteri
      (fun id color ->
         let s = graph.nodeLabel.(id) in
         match color with
         | Invisible -> ()
         | White -> Printf.fprintf out "\"%s\" [color=black]\n" s
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
