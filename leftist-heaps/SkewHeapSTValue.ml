(*
 * Skew Heaps with the original Sleator and Tarjan potential.
 *
 *)
{-# POTENTIAL (Tree Num: right_heavy) #-}
{-# VALUE_VARS #-}

{-# MODE worst_case #-}
min :: Tree Num -> Num | Tree Num [] -> Tree Num []
min x = match x with
  | leaf -> error
  | node t a u -> a

(*
insert ∷ (Num ⨯ Tree Num) → Tree Num | Tree Num [(x^1) ↦ 1, x ↦ 1, (2) |-> 3] → Tree Num [e1 ↦ 1]
insert a x = (meld (node leaf a leaf) x)
*)

delete_min ∷ Tree Num → Tree Num | Tree Num [x ↦ 1, (x^1) |-> 2, (e1^1) |-> 1] → Tree Num [e1 ↦ 1] 
delete_min x = match x with
  | leaf       → leaf
  | node t a u → meld t u

meld ∷ (Tree Num ⨯ Tree Num) → Tree Num | Tree Num [x ↦ 1, y ↦ 1, (x^1) |-> 1, (y^1) |-> 1, (e1^1) |-> 1] → Tree Num [e1 ↦ 1] 
meld x y = match x with
  | leaf          → y
  | node t a u → match y with
    | leaf             → (node t a u)
    | node v b w → if a <= b
      then node (~ meld (node v b w) u) a t
      else node (~ meld (node t a u) w) b v
