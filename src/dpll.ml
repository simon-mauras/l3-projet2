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
      getSolution stack
  end
