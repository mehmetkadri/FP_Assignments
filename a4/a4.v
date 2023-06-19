Module Q1.

Inductive letter: Type :=
  | A: letter
  | B: letter.

Inductive word: Type :=
  | Ew : word
  | Cw: letter -> word -> word.

Fixpoint append (w1 w2: word): word :=
  match w1 with
    | Ew      => w2
    | Cw l w => Cw l (append w w2)
  end.

Inductive pal_even: word -> Prop :=
  | pes : pal_even Ew
  | pnes: forall l w, pal_even w -> pal_even (Cw l (append w (Cw l Ew))).

Definition let_eqb (l1 l2 : letter): bool :=
  match l1, l2 with
    | A, A => true
    | B, B => true
    | _, _ => false
  end.

Fixpoint numA (w: word): nat :=
  match w with
    | Ew     => O
    | Cw l w => if let_eqb l A then S (numA w) else (numA w)
  end.

Fixpoint numB (w: word): nat :=
  match w with
    | Ew     => O
    | Cw l w => if let_eqb l B then S (numB w) else (numB w)
  end.

Lemma one: forall w, pal_even w -> Nat.even (numA w) = true.
Proof.
  (* your code here*)
Admitted.

End Q1.

Module Q2.

Require Import ZArith.

Definition mirror {A: Type} (R: A -> A -> Prop) := 
  forall x y, x = y <-> exists z: A, R x z /\ R z y.

Definition RZ: (Z -> Z -> Prop) := fun x y => y = Z.mul (-1) x.

Lemma two_a: mirror RZ.
Proof.
  (* your code here*)
Admitted.

Lemma two_b: forall {A: Type} (R: A -> A -> Prop), mirror R -> (forall x y, R x y -> R y x).
Proof.
  (* your code here*)
Admitted.

Definition RN: (nat -> nat -> Prop) := fun x y => x <> y. 

Lemma two_c_one: forall x y, RN x y -> RN y x.
Proof.
  (* your code here*)
Admitted.

Lemma two_c_two: mirror RN -> False.
Proof.
  (* your code here*)
Admitted.

End Q2.

Section Q3.

Definition LEM := forall P: Prop, P \/ ~ P.

Lemma three: forall (X: Type) (P: X -> Prop), LEM -> (~ (exists x: X, ~ P x) -> (forall x: X, P x)).
Proof.
  (* your code here*)
Admitted.

End Q3.
