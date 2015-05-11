Inductive formula : Set :=
  | Var : nat -> formula
  | And : formula -> formula -> formula
  | Or : formula -> formula -> formula
  | Imp : formula -> formula -> formula
  | Not : formula -> formula.

Fixpoint evaluate (v : nat -> bool) (f : formula) : bool :=
  match f with
  | Var n => v n
  | And a b => if evaluate v a then evaluate v b else false
  | Or a b => if evaluate v a then true else evaluate v b
  | Imp a b => if evaluate v b then evaluate v a else true
  | Not a => if evaluate v a then false else true
  end.

Definition f := And (Or (Var 1) (Not (Var 2))) (Var 2).
Print f.

Fixpoint v (n : nat) : bool :=
  match n with
  | 1 => true
  | 2 => true
  | _ => false
  end.

Lemma formula_is_sat : exists v, true = evaluate v f.
Proof.
  exists v.
  simpl.
  trivial.
Qed.

Check formula_is_sat.