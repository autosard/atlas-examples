(**
 * Basic building block of skew heaps, rotates along the rightmost path. 
 *
 *)
{-# POTENTIAL (Tree Base: logr) #-}

rot ∷ Tree Base → Tree Base | Tree Base [x ↦ 1, (x^1) ↦ 1] → Tree Base [e1 ↦ 1] 
rot x = match x with
  | leaf          → leaf
  | node t a u → node (~ rot u) a t
