(**
 * Basic building block of skew heaps, rotates along the rightmost path. 
 *
 *)
{-# POTENTIAL (Tree Base: logr) #-}

swap ∷ Tree Base → Tree Base | Tree Base [x ↦ 1, (x^1) ↦ 1] → Tree Base [e1 ↦ 1]
swap x = match x with
  | leaf          → leaf
  | node t a u → node (~ swap u) a t
