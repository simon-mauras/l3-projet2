(** Module implémentant l'algorithme DPLL *)

(** Module de type Sigs.Solver_type *)
module Make : Sigs.Solver_type =
  functor (Formula : Sigs.Formula_type) ->
  struct
    module Literal = Formula.Literal  
    module Graph = Graphe.Make(Formula)
    type assertion = Bet of Literal.t | Deduction of Literal.t
    
    let outDebug = ref stderr
    let setDebug b =
      outDebug := if b
        then stderr
        else open_out "/dev/null"
    
    let clauseLearning = ref false
    let setClauseLearning b = clauseLearning := b
    
    let clauseLearningInteractive = ref false
    let setClauseLearningInteractive b = clauseLearningInteractive := b
    
    
    let getSolution stack =
      let compare a b = (abs a) - (abs b) in
      let to_int = function
        | Bet x -> Literal.to_int x
        | Deduction x -> Literal.to_int x in
      let rec to_list s = if Stack.is_empty s
        then []
        else let a = Stack.pop s in (to_int a)::(to_list s) in
      let l = to_list stack in
      if l = [] then None else Some (List.sort compare l)

    (** Renvoie une solution à la formule donnée. Des informations de debug peuvent être afficher sur la sortie donnée *)
    let solve form =
      let stack = Stack.create () in
      let currentDeductionLevel = ref 0 in
      let deductionLevel = Array.make (2 * Formula.getNbVariables form) None in
      let deductionCause = Array.make (2 * Formula.getNbVariables form) None in
      let continue = ref true in
      let interactive = ref !clauseLearningInteractive in
      let statFreeLiteral = ref 0 in
      let statUnitClause = ref 0 in
      let statPureLiteral = ref 0 in
      while !continue
      do
        (* On vérifie que les hypothèses actuelles ne rendent pas la formule fausse *)
        if Formula.isFalse form then
          begin
            (*Printf.fprintf !outDebug "deductionLevel = %d\n" !currentDeductionLevel;
            for i=0 to (Array.length deductionCause) - 1 do
              Literal.print !outDebug (Literal.literal_of_id i);
              (match deductionCause.(i) with
              | None -> Printf.fprintf !outDebug " X"
              | Some i -> Printf.fprintf !outDebug " %d" i);
              (match deductionLevel.(i) with
              | None -> Printf.fprintf !outDebug " X\n"
              | Some i -> Printf.fprintf !outDebug " %d\n" i)
            done;*)
            
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
                  Graph.export outGraph (Graph.make form deductionCause deductionLevel !currentDeductionLevel) "G";
                  close_out outGraph;
                  print_endline "Graph has been exported.";
                | "c" -> ()
                | "t" -> interactive := false
                | _ -> choice () in
              choice ()
            end;
            
            let rec unstack () =
              if Stack.is_empty stack
              then continue := false
              else match Stack.pop stack with
                (* Si un paris et contradictoire, sa contradiction est une déduction *)
                | Bet x ->
                  Formula.forgetLiteral x form;
                  Formula.setLiteral (Literal.neg x) form;
                  Stack.push (Deduction (Literal.neg x)) stack;
                  decr currentDeductionLevel;
                  deductionLevel.(Literal.id_of_literal x) <- None (*Some !currentDeductionLevel*);
                | Deduction x ->
                  Formula.forgetLiteral x form;
                  deductionLevel.(Literal.id_of_literal x) <- None;
                  deductionCause.(Literal.id_of_literal x) <- None;
                  unstack();
            in
            unstack();
          end
        else
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
               Stack.push (Deduction x) stack;
               deductionCause.(Literal.id_of_literal x) <- Some i;
               deductionLevel.(Literal.id_of_literal x) <- Some !currentDeductionLevel);
            (*(* Propagation des litétaux purs *)
            (match Formula.getPureLiteral form with
             | None -> ()
             | Some x ->
               incr statPureLiteral;
               modif := true;
               Formula.setLiteral x form;
               Stack.push (Deduction x) stack);*)
            (* 2 : On parie sur un litéral si aucune modification n'a été faite *)
            if not !modif then
              (match Formula.getFreeLiteral form with
               | None -> continue := false (* La formule est satisfaite *)
               | Some x ->
                 incr currentDeductionLevel;
                 incr statFreeLiteral;
                 Formula.setLiteral x form;
                 Stack.push (Bet x) stack;
                 deductionLevel.(Literal.id_of_literal x) <- Some !currentDeductionLevel);
          end
      done;
      
      Printf.fprintf !outDebug "-------------------------------------\n";
      Printf.fprintf !outDebug "Nombre de paris freeLiteral : %d\n" !statFreeLiteral;
      Printf.fprintf !outDebug "Nombre de déductions UnitClause : %d\n" !statUnitClause;
      Printf.fprintf !outDebug "Nombre de déductions PureLiteral : %d\n" !statPureLiteral;
      Printf.fprintf !outDebug "-------------------------------------\n";
      
      (* A la fin de la boucle, la pile est soit vide (non satisfiable) soit contient toutes les variables *)
      getSolution stack
  end
