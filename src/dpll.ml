(** Module implémentant l'algorithme DPLL *)

(** Module de type Sigs.Solver_type *)
module Make : Sigs.Solver_type =
  functor (Formula : Sigs.Formula_type) ->
  struct
    module Literal = Formula.Literal    
    type assertion = Bet of Literal.t | Deduction of Literal.t

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
    let solve out form =
      let stack = Stack.create () in
      let continue = ref true in
      let statFreeLiteral = ref 0 in
      let statUnitClause = ref 0 in
      let statPureLiteral = ref 0 in
      while !continue
      do
        (* On vérifie que les hypothèses actuelles ne rendent pas la formule fausse *)
        if Formula.isFalse form then
          begin
            let rec unstack () =
              if Stack.is_empty stack
              then continue := false
              else match Stack.pop stack with
                (* Si un paris et contradictoire, sa contradiction est une déduction *)
                | Bet x ->
                  Formula.forgetLiteral x form;
                  Formula.setLiteral (Literal.neg x) form;
                  Stack.push (Deduction (Literal.neg x)) stack;
                | Deduction x ->
                  Formula.forgetLiteral x form;
                  unstack();
            in
            unstack()
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
               Stack.push (Deduction x) stack);
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
                 incr statFreeLiteral;
                 Formula.setLiteral x form;
                 Stack.push (Bet x) stack);
          end
      done;
      
      Printf.fprintf out "-------------------------------------\n";
      Printf.fprintf out "Nombre de paris freeLiteral : %d\n" !statFreeLiteral;
      Printf.fprintf out "Nombre de déductions UnitClause : %d\n" !statUnitClause;
      Printf.fprintf out "Nombre de déductions PureLiteral : %d\n" !statPureLiteral;
      Printf.fprintf out "-------------------------------------\n";
      
      (* A la fin de la boucle, la pile est soit vide (non satisfiable) soit contient toutes les variables *)
      getSolution stack
  end
