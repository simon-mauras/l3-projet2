(** Module implementant la manipulation a la volee du graphe des conflits *)

open Sigs

(** Module implementant le choix de la clause a apprendre *)
module Make (F: Formula_type) =
struct

  (** Type d'un graphe *)
  type t = {
    learnedClause : L.t list;
    uip : L.t;
  }
  
  let sort_uniq cmp l =
    let rec uniq res = function
    | a::b::l when (cmp a b) = 0 -> uniq res (b::l)
    | a::l -> uniq (a::res) l
    | [] -> res in
    uniq [] (List.sort cmp l)
  
  (** Construit un graphe (de type t) a partir d'un tableau de literaux (un noeud par literal) en y ajoutant un symbole d'absurdite *)
  let make conflict formula deductionCause deductionLevel currentDeductionLevel =
  
    let clause = ref [] in
    let uip = ref None in
    
    let fusion lit l1 l2 =
      let rec accumulate res = function
      | [] -> res
      | a::l when (a = lit || a = L.neg lit) -> accumulate res l
      | a::l -> accumulate (a::res) l in
      accumulate (accumulate [] l1) l2 in
    
    let rec explore cl =
      let conflict = sort_uniq (L.compare) cl in
      let level = List.filter (fun lit ->
                                 match deductionLevel.(L.id_of_literal (L.neg lit)) with
                                 | _, None -> failwith (Printf.sprintf "Error !")
                                 | _, Some l -> l = currentDeductionLevel) conflict in
                                
      let cause = List.filter (fun l -> deductionCause.(L.id_of_literal (L.neg l)) <> None) level in
      
      (*List.iter (fun l -> Printf.printf "%d " (L.to_int l)) level;
      Printf.printf "\n";
      List.iter (fun l -> Printf.printf "%d " (L.to_int l)) cause;
      Printf.printf "\n";*)
      
      match level, cause with
      | a::[],_ -> uip := Some (L.neg a);
                   clause := conflict;
      (*List.iter (fun l -> Printf.printf "%d " (L.to_int l)) conflict;
      Printf.printf "(%d)\n" (L.to_int a);*)
                   
      | _, [] -> failwith "There are multiple UIP..."
      | _, x::lst -> let next = List.fold_left (fun max lit -> let litLevel, _ = deductionLevel.(L.id_of_literal (L.neg lit)) in
                                                               let maxLevel, _ = deductionLevel.(L.id_of_literal (L.neg max)) in
                                                                match litLevel, maxLevel with
                                                               | None, _ -> failwith "Oups"
                                                               | _, None -> failwith "Oups"
                                                               | Some a, Some b -> if a > b then lit else max) x lst in
                    let litId = L.id_of_literal (L.neg next) in
                    let reasonId = match deductionCause.(litId) with
                                   | None -> failwith "This literal is true and it should have a cause."
                                   | Some i -> i in
                    let reason = F.getClause formula reasonId in
                    (*List.iter (fun l -> Printf.printf "%d " (L.to_int l)) reason;
                    Printf.printf "\n";
                    Printf.printf "%d\n" (L.to_int next);
                    Printf.printf "\n";*)
                    explore (fusion next conflict reason) in
    
    explore conflict;
    
    let u = match !uip with
            | None -> failwith "Uip not found"
            | Some i -> i in
    
    {  learnedClause = !clause; uip = u }

  let getLearnedClause graph = graph.learnedClause
  
  let getUip graph = graph.uip
  
end
