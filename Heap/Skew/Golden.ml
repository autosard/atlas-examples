(*
 * Skew Heaps with the optimal coefficients for golden ratio bound.
 *
 *)
{-# POTENTIAL (Tree Num: log_golden) #-}


min :: Tree Num -> Num @> Tree Num []
min x = match x with
  | leaf -> error
  | node t a u -> a

insert ∷ (Num ⨯ Tree Num) → Tree Num | Tree Num [(x^1,2) |-> 105/163, (x^1) ↦ 3115/7824, x ↦ 1, (2) |-> 23803/7824] → Tree Num [e1 ↦ 1]
insert a x = (meld (node leaf a leaf) x)


{-# STRONG_CF #-}
delete_min ∷ Tree Num → Tree Num | Tree Num [(x^1) |-> 5635/3912, x ↦ 1] → Tree Num [e1 ↦ 1]
delete_min x = match x with
  | leaf       → leaf
  | node t a u → meld t u

{-# NUM_CF_SIGS 2 #-}
meld ∷ (Tree Num ⨯ Tree Num) → Tree Num | Tree Num [x ↦ 1, y ↦ 1, (x^1,y^1,-1) ↦ 105/163, (x^1) |-> 3115/7824, (y^1) |-> 3115/7824] → Tree Num [e1 ↦ 1] 
meld x y = match x with
  | leaf          → y
  | node t a u → match y with
    | leaf             → (node t a u)
    | node v b w → if a <= b
      then bal t a (~ meld (node v b w) u)
      else bal v b (~ meld (node t a u) w)

{-# NUM_CF_SIGS 2 #-}
bal :: (Tree Num  * Num * Tree Num) -> Tree Num | Tree Num [t ↦ 1, u ↦ 1, (t^1,u^1) ↦ 105/163, (u^1) |-> -105/163] → Tree Num [e1 ↦ 1] 
bal t a u = node u a t
