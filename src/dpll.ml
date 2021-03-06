(** Module implementant l'algorithme DPLL *)

open Sigs
 
(** Module de type Solver_type *)
module Make : Solver_type =
  functor (H : Heuristic_type) ->
  functor (F : Formula_type) ->
  functor (T : Theory_type) ->
  struct
    module Heuristic = H
    module Formula = F
    module Theory = T
    module Graph = Graph.Make(F)
    module Graph2 = Graph2.Make(F)
    type assertion = Bet of L.t | Deduction of L.t
    
    let outDebug = ref stderr
    let clauseLearning = ref false
    let clauseLearningInteractive = ref false
    
    (** Active l'affichage d'informations de debug *)
    let setDebug b =
      outDebug := if b
        then stderr
        else open_out "/dev/null"
    
    (** Active l'apprentissage de clause *)
    let setClauseLearning b = clauseLearning := b
    
    (** Active l'apprentissage de clause interactif *)
    let setClauseLearningInteractive b = clauseLearningInteractive := b
    
    let getSolution stack =
      let compare a b = (abs a) - (abs b) in
      let to_int = function
        | Bet x -> L.to_int x
        | Deduction x -> L.to_int x in
      let rec to_list s = if Stack.is_empty s
        then []
        else let a = Stack.pop s in (to_int a)::(to_list s) in
      let l = to_list stack in
      if l = [] then None else Some (List.sort compare l)

    (** Renvoie une solution à la formule donnee. Des informations de debug peuvent être afficher sur la sortie donnee *)
    let solve data tab =
      let form = Formula.make !outDebug data in
      let heur = Heuristic.make !outDebug data in
      let theor = Theory.make tab in
      let stack = Stack.create () in
      let currentDeductionLevel = ref 0 in
      let currentLevel = ref 0 in
      let deductionLevel = Array.make (2 * Formula.getNbVariables form) (None, None) in
      let deductionCause = Array.make (2 * Formula.getNbVariables form) None in
      let continue = ref true in
      let interactive = ref !clauseLearningInteractive in
      let clauseLearning = !clauseLearningInteractive || !clauseLearning in
      let statFreeLiteral = ref 0 in
      let statUnitClause = ref 0 in
      let statPureLiteral = ref 0 in
      let statClauseLearning = ref 0 in
      while !continue
      do
        (* On verifie que les hypotheses actuelles ne rendent pas la formule fausse *)
        match if Formula.isFalse form
                then Formula.getConflict form
                else Theory.getContradiction theor
        with
        | Some conflict ->
          begin
            
            if !interactive then begin
              print_endline "Conflict found.";
              print_endline "- g : export .dot file with the conflict graph.";
              print_endline "- c : advance to the next conflict.";
              print_endline "- t : advance to the end.";
              let rec choice () = 
                print_string "Enter your choice (g/c/t) : ";
                match read_line () with
                | "g" ->
                  let outGraph = open_out "graph.dot" in
                  let graph = Graph.make conflict form deductionCause deductionLevel !currentDeductionLevel in
                  Graph.export outGraph graph "G";
                  close_out outGraph;
                  Printf.printf "Learned clause :";
                  List.iter (fun l -> Printf.printf " %d" (L.to_int l)) (Graph.getLearnedClause graph);
                  Printf.printf " 0\n";
                  print_endline "Graph has been exported.";
                | "c" -> ()
                | "t" -> interactive := false
                | _ -> choice () in
              choice ()
            end;
                                          
            let levelBacktrack = ref None in
            let deductionUip = ref None in
            let learnedClause = ref 0 in
            if clauseLearning then begin
              incr statClauseLearning;
              let graph = Graph2.make conflict form deductionCause deductionLevel !currentDeductionLevel in
              let clause = Graph2.getLearnedClause graph in
              let uip = Graph2.getUip graph in
              
              let lvl = List.fold_left (fun maxi l -> let id = L.id_of_literal (L.neg l) in
                                                      match deductionLevel.(id) with
                                                      | _, None -> failwith "Error: deductionLevel"
                                                      | _, Some x -> if l = L.neg uip then maxi else max x maxi) 0 clause in
                                                      
              if lvl <> !currentDeductionLevel
                then levelBacktrack := Some lvl;
              deductionUip := Some (L.neg uip);
              learnedClause := Formula.addClause clause form;
              Heuristic.addClause clause heur;
            end;
            
            let rec unstack () =
              if Stack.is_empty stack then begin
                currentDeductionLevel := 0;
                match !deductionUip with
                | None -> continue := false
                | Some u ->
                  Formula.setLiteral u form;
                  Heuristic.setLiteral u heur;
                  Theory.setConstraint u theor;
                  Stack.push (Deduction u) stack;
                  incr currentLevel;
                  deductionLevel.(L.id_of_literal u) <- Some !currentLevel, Some !currentDeductionLevel;
                  deductionCause.(L.id_of_literal u) <- Some !learnedClause;
              end else begin
                decr currentLevel;
                let s = Stack.pop stack in
                let x = match s with Bet l | Deduction l -> l in
                match s, deductionLevel.(L.id_of_literal x), !levelBacktrack  with
                | _, (_, None), _ -> failwith "Error: deductionLevel"
                | Bet x, (_, Some lvl), None ->
                  currentDeductionLevel := lvl;
                  Formula.forgetLiteral x form;
                  Heuristic.forgetLiteral x heur;
                  Theory.forgetConstraint x theor;
                  deductionLevel.(L.id_of_literal x) <- None, None;
                  deductionCause.(L.id_of_literal x) <- None;
                  Formula.setLiteral (L.neg x) form;
                  Heuristic.setLiteral (L.neg x) heur;
                  Theory.setConstraint (L.neg x) theor;
                  Stack.push (Deduction (L.neg x)) stack;
                  incr currentLevel;
                  deductionLevel.(L.id_of_literal (L.neg x)) <- Some !currentLevel, Some !currentDeductionLevel;
                  deductionCause.(L.id_of_literal (L.neg x)) <- None;
                | Deduction x, (_, Some lvl), None ->
                  currentDeductionLevel := lvl;
                  Formula.forgetLiteral x form;
                  Heuristic.forgetLiteral x heur;
                  Theory.forgetConstraint x theor;
                  deductionLevel.(L.id_of_literal x) <- None, None;
                  deductionCause.(L.id_of_literal x) <- None;
                  unstack();
                | Bet x, (_, Some lvl), Some lvlBacktrack
                | Deduction x, (_, Some lvl), Some lvlBacktrack ->
                  currentDeductionLevel := lvl;
                  if lvl > lvlBacktrack then begin
                    Formula.forgetLiteral x form;
                    Heuristic.forgetLiteral x heur;
                    Theory.forgetConstraint x theor;
                    deductionLevel.(L.id_of_literal x) <- None, None;
                    deductionCause.(L.id_of_literal x) <- None;
                    unstack ()
                  end else begin
                    Stack.push (Bet x) stack;
                    incr currentLevel;
                    match !deductionUip with
                    | None -> failwith "Error: deductionUip."
                    | Some u ->
                      Formula.setLiteral u form;
                      Heuristic.setLiteral u heur;
                      Theory.setConstraint u theor;
                      Stack.push (Deduction u) stack;
                      incr currentLevel;
                      deductionLevel.(L.id_of_literal u) <- Some !currentLevel, Some !currentDeductionLevel;
                      deductionCause.(L.id_of_literal u) <- Some !learnedClause;
                  end
                end
            in
            
            if !currentDeductionLevel = 0 then begin
              Stack.clear stack;
              continue := false;
            end else unstack();
            
          end
        | None ->
          begin
            (* 1 : On commence par simplifier la formule *)
            let modif = ref false in
            (* Propagation des clauses unitaires *)
            (match Formula.getUnitClause form with
             | None -> ()
             | Some (x,i) ->
               incr statUnitClause;
               modif := true;
               Formula.setLiteral x form;
               Heuristic.setLiteral x heur;
               Theory.setConstraint x theor;
               Stack.push (Deduction x) stack;
               incr currentLevel;
               deductionCause.(L.id_of_literal x) <- Some i;
               deductionLevel.(L.id_of_literal x) <- Some !currentLevel, Some !currentDeductionLevel);
            (* 2 : On parie sur un literal si aucune modification n'a ete faite *)
            if not !modif then
              (match Heuristic.getNextLiteral heur with
               | None -> continue := false (* La formule est satisfaite *)
               | Some x ->
                 incr currentDeductionLevel;
                 incr statFreeLiteral;
                 Formula.setLiteral x form;
                 Heuristic.setLiteral x heur;
                 Theory.setConstraint x theor;
                 Stack.push (Bet x) stack;
                 incr currentLevel;
                 deductionLevel.(L.id_of_literal x) <- Some !currentLevel, Some !currentDeductionLevel);
          end
      done;
      
      Printf.fprintf !outDebug "-------------------------------------\n";
      Printf.fprintf !outDebug "Nombre de paris freeLiteral : %d\n" !statFreeLiteral;
      Printf.fprintf !outDebug "Nombre de deductions UnitClause : %d\n" !statUnitClause;
      Printf.fprintf !outDebug "Nombre de deductions PureLiteral : %d\n" !statPureLiteral;
      Printf.fprintf !outDebug "Nombre de clause deduites ClauseLearning : %d\n" !statClauseLearning;
      Printf.fprintf !outDebug "-------------------------------------\n";
      
      (* A la fin de la boucle, la pile est soit vide (non satisfiable) soit contient toutes les variables *)
      getSolution stack
  end
