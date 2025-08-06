(**
 * Basic building block of skew heaps, rotates along the rightmost path. 
 *
 *)

{-# POTENTIAL (Tree Base: right_heavy) #-}

rot ∷ Tree Base → Tree Base | Tree Base [x ↦ 1, (x^1) ↦ 2] → Tree Base [e1 ↦ 1] 
rot x = match x with
  | leaf          → leaf
  | node t a u → node (~ rot u) a t
