{-# POTENTIAL (Tree Num: loglrx) #-}

{-# STRONG_CF #-}
delete_min ∷ Tree Num → Tree Num | Tree Num [(x^1) |-> 3/2, x ↦ 1] → Tree Num [e1 ↦ 1]
delete_min x = match x with
  | leaf       → leaf
  | node t a u → meld t u

{-# NUM_CF_SIGS 2 #-}
meld ∷ (Tree Num ⨯ Tree Num) → Tree Num | Tree Num [x ↦ 1, y ↦ 1, (x^1,y^1,-1) ↦ 1/2, (x^1) |-> 1/2, (y^1) |-> 1/2] → Tree Num [e1 ↦ 1] {Tree Num [(x^1,y^1,-1) ↦ 1/2] → Tree Num [(e1^1) |-> 1/2] ; Tree Num [(x^1,y^1,-1) ↦ -1/2] → Tree Num [(e1^1) |-> -1/2]}
meld x y = match x with
  | leaf       → y
  | node t a u → match y with
    | leaf          → (node t a u)
    | node v b w → if a <= b
      then if weight u <= weight t
           then node t a (~ meld u (node v b w))
           else node u a (~ meld t (node v b w))
      else if weight w <= weight v
           then node v b (~ meld w (node t a u))
           else node w b (~ meld v (node t a u))

