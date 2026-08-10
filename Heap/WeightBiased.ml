{-# POTENTIAL (Tree Base: log_golden) #-}

min :: Tree Base -> Base @> Tree Base []
min x = match x with
  | leaf -> error
  | node t a u -> a

insert ∷ (Base ⨯ Tree Base) → Tree Base | Tree Base [(x^1,2) |-> 105/163, (x^1) ↦ 3115/7824, x ↦ 1, (2) |-> 23803/7824] → Tree Base [e1 ↦ 1]
insert a x = (meld (node leaf a leaf) x)

delete_min ∷ Tree Base → Tree Base | Tree Base [(x^1) |-> 5635/3912, x ↦ 1] → Tree Base [e1 ↦ 1]
delete_min x = match x with
  | leaf       → leaf
  | node t a u → meld t u

{-# NUM_CF_SIGS 2 #-}
meld ∷ (Tree Base ⨯ Tree Base) → Tree Base | Tree Base [x ↦ 1, y ↦ 1, (x^1,y^1,-1) ↦ 105/163, (x^1) |-> 3115/7824, (y^1) |-> 3115/7824] → Tree Base [e1 ↦ 1] 
meld x y = match x with
  | leaf       → y
  | node t a u → match y with
    | leaf          → (node t a u)
    | node v b w → if a <= b
      then bal t a (~ meld u (node v b w))
      else bal v b (~ meld w (node t a u))

{-# NUM_CF_SIGS 2 #-}
bal :: (Tree Base  * Base * Tree Base) -> Tree Base | Tree Base [t ↦ 1, u ↦ 1, (t^1,u^1) ↦ 105/163, (u^1) |-> -105/163] → Tree Base [e1 ↦ 1] 
bal t a u = if weight t <= weight u
  then (node u a t)
  else (node t a u)



