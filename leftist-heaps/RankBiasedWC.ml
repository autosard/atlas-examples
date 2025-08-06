{-# POTENTIAL (Tree Base: rank) #-}
{-# RHSTERMS #-}

min :: Tree Base -> Base | Tree Base [] -> Tree Base []
min x = match x with
  | leaf -> error
  | node t a u -> a

(*
insert ∷ (Base ⨯ Tree Base) → Tree Base | Tree Base [(e1^1) |-> 1/2, (x^1) ↦ 1, x ↦ 1, (2) |-> 3] → Tree Base [e1 ↦ 1, (2) |-> 1]
insert a x = (meld (node leaf a leaf) x)
*)

(*
delete_min ∷ Tree Base → Tree Base @> Tree Base [x |-> 2]
delete_min x = match x with
  | leaf       → leaf
  | node t a u → meld t u
*)

meld :: (Tree Base * Tree Base) -> Tree Base @> Tree Base [x ↦ 1, y ↦ 1] 
meld x y = match x with
  | leaf       → y
  | node t a u → match y with
    | leaf          → (node t a u)
    | node v b w → if a <= b
      then bal t a (~ meld u (node v b w))
      else bal v b (~ meld w (node t a u))


bal :: (Tree Base  * Base * Tree Base) -> Tree Base @> Tree Base [] 
bal t a u = if rank t <= rank u
  then (node u a t)
  else (node t a u)



