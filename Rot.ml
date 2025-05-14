(**
 * Basic building block of skew heaps, rotates along the rightmost path. 
 *
 * )

{-# POTENTIAL (Tree Base: logr) #-}

rot ∷ Tree Base → Tree Base | Tree Base [t ↦ 1, (t^1) ↦ 1] → Tree Base [e1 ↦ 1] 
rot t = match t with
  | leaf          → leaf
  | node l a r → node (~ rot r) a l
