{-# POTENTIAL (Tree Base: rank) #-}

{-# MODE worst_case #-}
min :: Tree Base -> Base | Tree Base [] -> Tree Base []
min x = match x with
  | leaf -> error
  | node t a u -> a


(*
insert ∷ (Base ⨯ Tree Base) → Tree Base | Tree Base [(2) |-> 0, (x^1, 2) |-> 2, x |-> 1] -> Tree Base [e1 ↦ 1]
insert a x = (meld (node leaf a leaf) x)
*)

delete_min ∷ Tree Base → Tree Base | Tree Base [(x^1) |-> 2, x |-> 1] -> Tree Base [e1 ↦ 1]
delete_min x = match x with
  | leaf       → leaf
  | node t a u → meld t u


{-# MODE hybrid #-}
meld :: (Tree Base * Tree Base) -> Tree Base | Tree Base [x ↦ 1, y ↦ 1, (x^1,y^1) ↦ 1] → Tree Base [e1 ↦ 1] ; Tree Base [x ↦ 1, y ↦ 1] → Tree Base [] 
meld x y = match x with
  | leaf       → y
  | node t a u → match y with
    | leaf          → (node t a u)
    | node v b w → if a <= b
      then bal t a (~ meld u (node v b w))
      else bal v b (~ meld w (node t a u))

{-# MODE worst_case #-}
bal :: (Tree Base  * Base * Tree Base) -> Tree Base | Tree Base [] → Tree Base [] {Tree Base [(t^1,u^1) ↦ 1] → Tree Base [(e1^1) |-> 1]}
bal t a u = if rank t <= rank u
  then (node u a t)
  else (node t a u)
