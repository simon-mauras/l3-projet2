(**********************************************************************)
(**********************************************************************)
(************************ DEFINITIONS GENERALES ***********************)
(**********************************************************************)
(**********************************************************************)

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
  | Imp a b => if evaluate v a then evaluate v b else true
  | Not a => if evaluate v a then false else true
  end.

Inductive var : Set :=
  | Leaf_var : nat -> var
  | Root_var : var
  | Left_var : var -> var
  | Right_var : var -> var.

Inductive literal : Set :=
  | Pos : var -> literal
  | Neg : var -> literal.

Require Import List.

Definition clause := list literal.

Definition cnf := list clause.

Fixpoint evaluate_literal (v : var -> bool) (l : literal) : bool :=
  match l with
  | Pos x => v x
  | Neg x => if v x then false else true
  end.

Fixpoint evaluate_clause (v : var -> bool) (c : clause) : bool := 
  match c with
  | nil => false
  | a::l => if evaluate_literal v a then true else evaluate_clause v l
  end.

Fixpoint evaluate_cnf (v : var -> bool) (c : cnf) : bool := 
  match c with
  | nil => true
  | a::l => if evaluate_clause v a then evaluate_cnf v l else false
  end.

Fixpoint concat (a : cnf) (b : cnf) :=
  match b with
  | nil => a
  | x :: l => x :: (concat a l)
  end.

Fixpoint add_right_var (a : var) : var :=
  match a with
  | Leaf_var n => Leaf_var n
  | x => Right_var x
  end.

Fixpoint add_left_var (a : var) : var :=
  match a with
  | Leaf_var n => Leaf_var n
  | x => Left_var x
  end.

Fixpoint add_right_literal (a : literal) : literal :=
  match a with
  | Pos x => Pos (add_right_var x)
  | Neg x => Neg (add_right_var x)
  end.

Fixpoint add_left_literal (a : literal) : literal :=
  match a with
  | Pos x => Pos (add_left_var x)
  | Neg x => Neg (add_left_var x)
  end.

Fixpoint add_right_clause (a : clause) : clause :=
  match a with
  | nil => nil
  | x :: l => (add_right_literal x) :: (add_right_clause l)
  end.

Fixpoint add_left_clause (a : clause) : clause :=
  match a with
  | nil => nil
  | x :: l => (add_left_literal x) :: (add_left_clause l)
  end.

Fixpoint add_right_cnf (a : cnf) : cnf :=
  match a with
  | nil => nil
  | x :: l => (add_right_clause x) :: (add_right_cnf l)
  end.

Fixpoint add_left_cnf (a : cnf) : cnf :=
  match a with
  | nil => nil
  | x :: l => (add_left_clause x) :: (add_left_cnf l)
  end.

Fixpoint aux_tseitin (f : formula) : cnf :=
  match f with
  | Var n => (Neg Root_var :: Pos (Leaf_var n) :: nil)
           ::(Pos Root_var :: Neg (Leaf_var n) :: nil)
           :: nil
  | And a b => let x := Root_var in
               let xa := Left_var Root_var in
               let xb := Right_var Root_var in
               let la := add_left_cnf (aux_tseitin a) in
               let lb := add_right_cnf (aux_tseitin b) in
               let cl1 := (Pos x)::(Neg xa)::(Neg xb)::nil in
               let cl2 := (Neg x)::(Pos xa)::nil in
               let cl3 := (Neg x)::(Pos xb)::nil in
               cl1 :: cl2 :: cl3 :: (concat la lb)
  | Or a b => let x := Root_var in
              let xa := Left_var Root_var in
              let xb := Right_var Root_var in
              let la := add_left_cnf (aux_tseitin a) in
              let lb := add_right_cnf (aux_tseitin b) in
              let cl1 := (Neg x)::(Pos xa)::(Pos xb)::nil in
              let cl2 := (Pos x)::(Neg xa)::nil in
              let cl3 := (Pos x)::(Neg xb)::nil in
              cl1 :: cl2 :: cl3 :: (concat la lb)
  | Imp a b => let x := Root_var in
               let xa := Left_var Root_var in
               let xb := Right_var Root_var in
               let la := add_left_cnf (aux_tseitin a) in
               let lb := add_right_cnf (aux_tseitin b) in
               let cl1 := (Neg x)::(Neg xa)::(Pos xb)::nil in
               let cl2 := (Pos x)::(Pos xa)::nil in
               let cl3 := (Pos x)::(Neg xb)::nil in
               cl1 :: cl2 :: cl3 :: (concat la lb)
  | Not a => let x := Root_var in
             let xa := Left_var Root_var in
             let la := add_left_cnf (aux_tseitin a) in
             let cl1 := (Neg x)::(Neg xa)::nil in
             let cl2 := (Pos x)::(Pos xa)::nil in
             cl1 :: cl2 :: la
  end.

Definition tseitin (f : formula) :=
  (Pos Root_var :: nil) :: (aux_tseitin f).

(**********************************************************************)
(**********************************************************************)
(************************ LEMMES PRELIMINAIRES ************************)
(**********************************************************************)
(**********************************************************************)

Lemma concat_cnf_left : forall v, forall a b,
  (true = evaluate_cnf v (concat a b)) -> (true = evaluate_cnf v a).
Proof.
  intros.
  induction b.
  apply H.
  apply IHb.
  replace (concat a (a0 :: b)) with (a0 :: (concat a b)) in H.
  simpl in H.
  destruct (evaluate_clause v a0).
  trivial.
  inversion H.
  trivial.
Qed.


Lemma concat_cnf_right : forall v, forall a b,
  (true = evaluate_cnf v (concat a b)) -> (true = evaluate_cnf v b).
Proof.
  intros.
  induction b.
  trivial.
  replace (concat a (a0 :: b)) with (a0 :: (concat a b)) in H.
  simpl in H.
  simpl.
  destruct (evaluate_clause v a0).
  apply IHb.
  trivial.
  trivial.
  trivial.
Qed.

Lemma concat_cnf : forall v, forall a b,
  (true = evaluate_cnf v a) -> (true = evaluate_cnf v b) -> (true = evaluate_cnf v (concat a b)).
Proof.
  intros.
  induction b.
  trivial.
  simpl.
  simpl in H0.
  destruct (evaluate_clause v a0).
  apply IHb.
  trivial.
  trivial.
Qed.

Lemma add_left_clause_sat : forall v, forall l,
  (true = evaluate_clause (fun x => match x with
                                    | Leaf_var n => v x
                                    | _ => v (Left_var x)
                                    end) l)
  -> (true = evaluate_clause v (add_left_clause l)).
Proof.
  intros.
  induction l.
  trivial.
  destruct a.
  destruct v0.

  simpl.
  simpl in H.
  destruct (v (Leaf_var n)).
  trivial.
  apply IHl.
  trivial.
  
  simpl.
  simpl in H.
  destruct (v (Left_var Root_var)).
  trivial.
  apply IHl.
  trivial.

  simpl.
  simpl in H.
  destruct (v (Left_var (Left_var v0))).
  trivial.
  apply IHl.
  trivial.

  simpl.
  simpl in H.
  destruct (v (Left_var (Right_var v0))).
  trivial.
  apply IHl.
  trivial.
  
  destruct v0.
  
  simpl.
  simpl in H.
  destruct (v (Leaf_var n)).
  apply IHl.
  trivial.
  trivial.
  
  simpl.
  simpl in H.
  destruct (v (Left_var Root_var)).
  apply IHl.
  trivial.
  trivial.

  simpl.
  simpl in H.
  destruct (v (Left_var (Left_var v0))).
  apply IHl.
  trivial.
  trivial.

  simpl.
  simpl in H.
  destruct (v (Left_var (Right_var v0))).
  apply IHl.
  trivial.
  trivial.
Qed.

Lemma add_left_cnf_sat : forall v, forall l,
  (true = evaluate_cnf (fun x => match x with
                                    | Leaf_var n => v x
                                    | _ => v (Left_var x)
                                    end) l)
  -> (true = evaluate_cnf v (add_left_cnf l)).
Proof.
  intros.
  induction l.
  trivial.
  simpl.
  simpl in H.
  cut (true = evaluate_clause
              (fun x => match x with
                        | Leaf_var _ => v x
                        | _ => v (Left_var x)
                        end) a).
  intro.
  rewrite <- H0 in H.
  cut (true = evaluate_clause v (add_left_clause a)).
  intro.
  rewrite <- H1.
  apply IHl.
  trivial.
  apply add_left_clause_sat.
  trivial.
  destruct (evaluate_clause
               (fun x : var =>
               match x with
               | Leaf_var _ => v x
               | Root_var => v (Left_var x)
               | Left_var _ => v (Left_var x)
               | Right_var _ => v (Left_var x)
               end) a).
  trivial.
  trivial.
Qed.


Lemma add_right_clause_sat : forall v, forall l,
  (true = evaluate_clause (fun x => match x with
                                    | Leaf_var n => v x
                                    | _ => v (Right_var x)
                                    end) l)
  -> (true = evaluate_clause v (add_right_clause l)).
Proof.
  intros.
  induction l.
  trivial.
  destruct a.

  destruct v0.

  simpl.
  simpl in H.
  destruct (v (Leaf_var n)).
  trivial.
  apply IHl.
  trivial.
  
  simpl.
  simpl in H.
  destruct (v (Right_var Root_var)).
  trivial.
  apply IHl.
  trivial.

  simpl.
  simpl in H.
  destruct (v (Right_var (Left_var v0))).
  trivial.
  apply IHl.
  trivial.

  simpl.
  simpl in H.
  destruct (v (Right_var (Right_var v0))).
  trivial.
  apply IHl.
  trivial.

  destruct v0.
  
  simpl.
  simpl in H.
  destruct (v (Leaf_var n)).
  apply IHl.
  trivial.
  trivial.
  
  simpl.
  simpl in H.
  destruct (v (Right_var Root_var)).
  apply IHl.
  trivial.
  trivial.

  simpl.
  simpl in H.
  destruct (v (Right_var (Left_var v0))).
  apply IHl.
  trivial.
  trivial.

  simpl.
  simpl in H.
  destruct (v (Right_var (Right_var v0))).
  apply IHl.
  trivial.
  trivial.
Qed.

Lemma add_right_cnf_sat : forall v, forall l,
  (true = evaluate_cnf (fun x => match x with
                                    | Leaf_var n => v x
                                    | _ => v (Right_var x)
                                    end) l)
  -> (true = evaluate_cnf v (add_right_cnf l)).
Proof.
  intros.
  induction l.
  trivial.
  simpl.
  simpl in H.
  cut (true = evaluate_clause
              (fun x => match x with
                        | Leaf_var _ => v x
                        | _ => v (Right_var x)
                        end) a).
  intro.
  rewrite <- H0 in H.
  cut (true = evaluate_clause v (add_right_clause a)).
  intro.
  rewrite <- H1.
  apply IHl.
  trivial.
  apply add_right_clause_sat.
  trivial.
  destruct (evaluate_clause
               (fun x : var =>
               match x with
               | Leaf_var _ => v x
               | Root_var => v (Right_var x)
               | Left_var _ => v (Right_var x)
               | Right_var _ => v (Right_var x)
               end) a).
  trivial.
  trivial.
Qed.

Lemma cong_valuation : forall v1 v2, (forall x, v1 x = v2 x)
  -> (forall l, evaluate_cnf v1 l = evaluate_cnf v2 l).
Proof.
  intros.
  induction l.
  trivial.
  simpl.
  rewrite <- IHl.
  cut (evaluate_clause v1 a = evaluate_clause v2 a).
  intro.
  rewrite <- H0.
  trivial.
  induction a.
  trivial.
  simpl.
  cut (evaluate_literal v1 a = evaluate_literal v2 a).
  intro.
  rewrite <- H0.
  rewrite <- IHa.
  trivial.
  destruct a.
  simpl.
  trivial.
  simpl.
  let x := v1 in rewrite <- H.
  trivial.
Qed.

(**********************************************************************)
(**********************************************************************)
(****************************** THEOREMES *****************************)
(**********************************************************************)
(**********************************************************************)

Lemma aux_tseitin_correct : forall f, forall v, exists v',
   (v = fun n => v' (Leaf_var n))
/\ (true = evaluate_cnf v' (aux_tseitin f))
/\ (evaluate v f = v' Root_var).
Proof.
  intro f.
  intro v.
  induction f.

  (*****************************************************************)
  (*****************************   VAR  ****************************)
  (*****************************************************************)
  
  exists (fun x => match x with Leaf_var y => v y | _ => v n end).
  split.
  trivial.
  split.
  simpl.
  destruct (v n).
  trivial.
  trivial.
  trivial.

  (*****************************************************************)
  (************************   LEMME UTILE  *************************)
  (*****************************************************************)

  Lemma interm : forall f1 f2 v v1 v2 b,
  v = (fun n : nat => v1 (Leaf_var n)) ->
  v = (fun n : nat => v2 (Leaf_var n)) ->
  true = evaluate_cnf v1 (aux_tseitin f1) -> 
  true = evaluate_cnf v2 (aux_tseitin f2) ->
  true = evaluate_cnf
           (fun x : var =>
            match x with
            | Leaf_var n => v n
            | Root_var => b
            | Left_var y => v1 y
            | Right_var y => v2 y
            end)
           (concat (add_left_cnf (aux_tseitin f1)) (add_right_cnf (aux_tseitin f2))).
  Proof.
    intros.
    apply concat_cnf.
    apply add_left_cnf_sat.
    rewrite <- cong_valuation with
    (v1 := v1)
    (v2 := fun x : var =>
           match x with
           | Leaf_var _ =>
             match x with
             | Leaf_var n0 => v n0
             | Root_var => b
             | Left_var y => v1 y
             | Right_var y => v2 y
             end
           | Root_var => v1 x
           | Left_var _ => v1 x
           | Right_var _ => v1 x
           end).
     trivial.
    intro.
    destruct x.
    rewrite -> H.
    trivial.
    trivial.
    trivial.
    trivial.
    apply add_right_cnf_sat.
    rewrite <- cong_valuation with
    (v1 := v2)
    (v2 := fun x : var =>
           match x with
           | Leaf_var _ =>
             match x with
             | Leaf_var n0 => v n0
             | Root_var => b
             | Left_var y => v1 y
             | Right_var y => v2 y
             end
           | Root_var => v2 x
           | Left_var _ => v2 x
           | Right_var _ => v2 x
           end).
    trivial.
    intro.
    destruct x.
    rewrite -> H0.
    trivial.
    trivial.
    trivial.
    trivial.
  Qed.

  (*****************************************************************)
  (*****************************   AND  ****************************)
  (*****************************************************************)

  destruct IHf1 as (v1, IHf1).
  destruct IHf1 as (IHf1a, IHf1).
  destruct IHf1 as (IHf1b, IHf1c).
  destruct IHf2 as (v2, IHf2).
  destruct IHf2 as (IHf2a, IHf2).
  destruct IHf2 as (IHf2b, IHf2c).

  exists (fun x => match x with
          | Leaf_var n => v n
          | Root_var => if v1 Root_var then v2 Root_var else false
          | Left_var y => v1 y
          | Right_var y => v2 y
          end).
  
  split.
  trivial.
  split.

  simpl.
  destruct (v1 Root_var).
  destruct (v2 Root_var).
  apply interm; trivial.
  apply interm; trivial.
  apply interm; trivial.
  simpl.
  rewrite <- IHf2c.
  rewrite <- IHf1c.
  trivial.
  
  (*****************************************************************)
  (*****************************   OR  ****************************)
  (*****************************************************************)

  destruct IHf1 as (v1, IHf1).
  destruct IHf1 as (IHf1a, IHf1).
  destruct IHf1 as (IHf1b, IHf1c).
  destruct IHf2 as (v2, IHf2).
  destruct IHf2 as (IHf2a, IHf2).
  destruct IHf2 as (IHf2b, IHf2c).

  exists (fun x => match x with
          | Leaf_var n => v n
          | Root_var => if v1 Root_var then true else v2 Root_var
          | Left_var y => v1 y
          | Right_var y => v2 y
          end).
  
  split.
  trivial.
  split.
  simpl.
  destruct (v1 Root_var).
  apply interm; trivial.
  destruct (v2 Root_var).
  apply interm; trivial.
  apply interm; trivial.
  simpl.
  rewrite <- IHf2c.
  rewrite <- IHf1c.
  trivial.

  (*****************************************************************)
  (****************************   IMPL  ****************************)
  (*****************************************************************)

  destruct IHf1 as (v1, IHf1).
  destruct IHf1 as (IHf1a, IHf1).
  destruct IHf1 as (IHf1b, IHf1c).
  destruct IHf2 as (v2, IHf2).
  destruct IHf2 as (IHf2a, IHf2).
  destruct IHf2 as (IHf2b, IHf2c).

  exists (fun x => match x with
          | Leaf_var n => v n
          | Root_var => if v1 Root_var then v2 Root_var else true
          | Left_var y => v1 y
          | Right_var y => v2 y
          end).
  
  split.
  trivial.
  split.
  simpl.
  destruct (v1 Root_var).
  destruct (v2 Root_var).
  apply interm; trivial.
  apply interm; trivial.
  apply interm; trivial.
  simpl.
  rewrite <- IHf2c.
  rewrite <- IHf1c.
  trivial.

  (*****************************************************************)
  (*****************************   NOT  ****************************)
  (*****************************************************************)

  destruct IHf as (v', IHf).
  destruct IHf as (IHfa, IHf).
  destruct IHf as (IHfb, IHfc).

  exists (fun x => match x with
          | Leaf_var n => v n
          | Root_var => if v' Root_var then false else true
          | Left_var y => v' y
          | Right_var y => v' y
          end).

  split.
  trivial.
  split.
  simpl.
  destruct (v' Root_var).
  apply add_left_cnf_sat.
  rewrite <- cong_valuation with
  (v1 := v')
  (v2 := fun x : var =>
         match x with
         | Leaf_var _ =>
           match x with
           | Leaf_var n0 => v n0
           | Root_var => false
           | Left_var y => v' y
           | Right_var y => v' y
           end
         | Root_var => v' x
         | Left_var _ => v' x
         | Right_var _ => v' x
         end)
  (l := aux_tseitin f).
  trivial.
  intro.
  destruct x.
  rewrite -> IHfa.
  trivial.
  trivial.
  trivial.
  trivial.
  apply add_left_cnf_sat.
  rewrite <- cong_valuation with
  (v1 := v')
  (v2 := fun x : var =>
         match x with
         | Leaf_var _ =>
           match x with
           | Leaf_var n0 => v n0
           | Root_var => true
           | Left_var y => v' y
           | Right_var y => v' y
           end
         | Root_var => v' x
         | Left_var _ => v' x
         | Right_var _ => v' x
         end)
  (l := aux_tseitin f).
  trivial.
  intro.
  destruct x.
  rewrite -> IHfa.
  trivial.
  trivial.
  trivial.
  trivial.
  destruct (v' Root_var).
  simpl.
  rewrite -> IHfc.
  trivial.
  simpl.
  rewrite -> IHfc.
  trivial.
Qed.

Lemma tseitin_unsat : forall f,
  (forall v', false = evaluate_cnf v' (tseitin f)) -> (forall v, false = evaluate v f).
Proof.
  intros.
  cut (exists v' : var -> bool,
         v = (fun n : nat => v' (Leaf_var n)) /\
         true = evaluate_cnf v' (aux_tseitin f) /\
         evaluate v f = v' Root_var).
  intro.
  destruct H0.
  destruct H0.
  destruct H1.
  rewrite -> H2.
  cut (false = evaluate_cnf x (tseitin f)).
  intro.
  simpl in H3.
  destruct (x Root_var).
  rewrite -> H3.
  rewrite -> H1.
  trivial.
  trivial.
  apply H.
  apply aux_tseitin_correct.
Qed.


Definition f :=
And (Var 1) (Not (Var 1))
.

Print f.

Let c := tseitin f.

Lemma cnf_is_unsat : forall v, false = evaluate_cnf v c.
Proof.

  unfold c.
  unfold f.
  unfold tseitin.
  unfold aux_tseitin.
  unfold add_left_cnf.
  unfold add_left_clause.
  unfold add_left_literal.
  unfold add_left_var.
  unfold add_right_cnf.
  unfold add_right_clause.
  unfold add_right_literal.
  unfold add_right_var.
  unfold concat.
  unfold evaluate_cnf.
  
  intro.
  
  Ltac reduce :=
    match goal with
    | [ |- false = if ?X then ?Y else false ] => destruct X eqn : ?Cl ; trivial
    end.
  repeat reduce.

  Fixpoint concat_clause (a : clause) (b : clause) :=
    match b with
    | nil => a
    | x :: l => x :: (concat_clause a l)
    end.
    
  Lemma concat_clause_left : forall v cl1 cl2,
    evaluate_clause v cl1 = true -> evaluate_clause v (concat_clause cl1 cl2) = true.
  Proof.
    intros.
    induction cl2.
    trivial.
    simpl.
    destruct (evaluate_literal v a); trivial.
  Qed.

  Lemma concat_clause_right : forall v cl1 cl2,
    evaluate_clause v cl2 = true -> evaluate_clause v (concat_clause cl1 cl2) = true.
  Proof.
    intros.
    induction cl2.
    simpl in H.
    inversion H.
    simpl.
    simpl in H.
    destruct (evaluate_literal v a).
    trivial.
    apply IHcl2.
    trivial.
  Qed.

  Lemma deduction : forall v x cl1 cl2,
    evaluate_clause v ((Pos x) :: cl1) = true->
    evaluate_clause v ((Neg x) :: cl2) = true->
    evaluate_clause v (concat_clause cl1 cl2) = true.
  Proof.
    intros.
    simpl in H.
    simpl in H0.
    destruct (v x).
    apply concat_clause_right; trivial.
    apply concat_clause_left; trivial.
  Qed.

  cut (evaluate_clause v (Pos (Left_var Root_var) :: nil) = true).
  intros ?Cl.
  cut (evaluate_clause v (Pos (Right_var Root_var) :: nil) = true).
  intros ?Cl.
  cut (evaluate_clause v (Neg (Right_var (Left_var Root_var)) :: nil) = true).
  intros ?Cl.
  cut (evaluate_clause v (Neg (Leaf_var 1) :: nil) = true).
  intros ?Cl.
  cut (evaluate_clause v (Pos (Leaf_var 1) :: nil) = true).
  intros ?Cl.
  cut (evaluate_clause v nil = true).
  trivial.

  apply deduction with
  (x := Leaf_var 1)
  (cl1 := nil) 
  (cl2 := nil); trivial.
  
  apply deduction with
  (x := Left_var Root_var)
  (cl1 := nil)
  (cl2 := Pos (Leaf_var 1) :: nil); trivial.
  
  apply deduction with
  (x := Right_var (Left_var Root_var))
  (cl1 := Neg (Leaf_var 1) :: nil)
  (cl2 := nil); trivial.
  
  apply deduction with
  (x := Right_var Root_var)
  (cl1 := nil)
  (cl2 := Neg (Right_var (Left_var Root_var)) :: nil); trivial.
  
  apply deduction with
  (x := Root_var)
  (cl1 := nil)
  (cl2 := Pos (Right_var Root_var) :: nil); trivial.
  
  apply deduction with
  (x := Root_var)
  (cl1 := nil)
  (cl2 := Pos (Left_var Root_var) :: nil); trivial.
Qed.


Theorem formula_is_unsat : forall v, false = evaluate v f.
Proof.
  apply tseitin_unsat.
  apply cnf_is_unsat.
Qed.

Check formula_is_unsat.
