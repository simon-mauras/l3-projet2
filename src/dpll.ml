module Make : Sigs.Solver_type =
  functor (Formula : Sigs.Formula_type) ->
  struct
    module Literal = Formula.Literal    
    type assertion = Bet of Literal.t | Deduction of Literal.t

    let solve out form =
      let stack = Stack.create () in
      let continue = ref true in
      while !continue
      do
        (* Check if formula's value is already determined *)
        if Formula.isFalse form then
          begin
            let rec unstack () =
              if Stack.is_empty stack
              then continue := false
              else match Stack.pop stack with
                (* If it's a bet, we try the other possibility *)
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
            (* First, simplify the formula *)
            let modif = ref false in
            (* Unit clause propagation *)
            (match Formula.getUnitClause form with
             | None -> ()
             | Some x ->
               modif := true;
               Formula.setLiteral x form;
               Stack.push (Deduction x) stack);
            (* Pure literal propagation *)
            (match Formula.getPureLiteral form with
             | None -> ()
             | Some x ->
               modif := true;
               Formula.setLiteral x form;
               Stack.push (Deduction x) stack);
            (* Bet on a literal if there is no modification *)
            if not !modif then
              (match Formula.getFreeLiteral form with
               | None -> continue := false (* La formule est satisfaite *)
               | Some x ->
                 Formula.setLiteral x form;
                 Stack.push (Bet x) stack);
          end
      done;
      if Stack.is_empty stack then None else Some []
  end
