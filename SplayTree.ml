(**
 * The function definitions in this file are taken from or made to match
 * Section 6 of
 *
 *   Tobias Nipkow, Hauke Brinkop
 *   Amortized Complexity Verified
 *   Journal of Automated Reasoning, Vol. 62, Iss. 3, pp. 367-391
 *   https://doi.org/10.1007/s10817-018-9459-3
 *   https://dblp.org/rec/journals/jar/NipkowB19
 *)
{-# POTENTIAL (Tree Base: loglr, List Base: linlog) #-}

splay ∷ (Base ⨯ Tree Base) → Tree Base | Tree Base [t ↦ 1/2, (t^1) ↦ 3/2] → Tree Base [e1 ↦ 1/2]
(*{Tree Base [(t^1) ↦ 1/2] → Tree Base [(e1^1) ↦ 1/2]} *)
(* splay ∷ (Base ⨯ Tree Base) → Tree Base @ Tree Base [(t^1) |-> 3/2] *)
splay a t = match t with
  | leaf -> leaf
  | node cl c cr → if a == c
    then node cl c cr
    else if a < c
      then match cl with
        | leaf         → node leaf c cr
        | node bl b br → if a == b
          then node bl a (node br c cr)
          else if a < b
            then match bl with
              | leaf → node leaf b (node br c cr)
              | bl   → match ~ splay a bl with
                | leaf -> leaf
                | node al _ ar → node al a (node ar b (node br c cr))
            else match br with
              | leaf → node bl b (node leaf c cr)
              | br   → match ~ splay a br with
                | leaf -> leaf
                | node al _ ar → node (node bl b al) a (node ar c cr)
      else match cr with
        | leaf        → node cl c leaf
        | node bl b br → if a == b
          then node (node cl c bl) a br
          else if a < b
            then match bl with
              | leaf → node (node cl c leaf) b br
              | bl   → match ~ splay a bl with
                | leaf -> leaf
                | node al _ ar → node (node cl c al) a (node ar b br)
            else match br with
              | leaf → node (node cl c bl) b leaf
              | br   → match ~ splay a br with
                | leaf -> leaf
                | node al _ ar → node (node (node cl c bl) b al) a ar

splay_max ∷ Tree Base → Tree Base | Tree Base [t ↦ 1/2, (t^1) ↦ 3/2] → Tree Base [e1 ↦ 1/2] {Tree Base [(t^1) ↦ 1/2] → Tree Base [(e1^1) ↦ 1/2]} 
(* splay_max ∷ Tree Base → Tree Base @ Tree Base [(t^1) |-> 3/2]*)
splay_max t = match t with
  | leaf -> leaf
  | node l b r → match r with
    | leaf         → node l b leaf
    | node rl c rr → match rr with
      | leaf → node (node l b rl) c leaf
      | rr   → match ~ splay_max rr with
        | leaf -> leaf
        | node rrl1 x xa → node (node (node l b rl) c rrl1) x xa



delete ∷ (Base ⨯ Tree Base) → Tree Base | Tree Base [t ↦ 1/2, (2) ↦ 2, (t^1) ↦ 5/2] → Tree Base [e1 ↦ 1/2]
(* delete ∷ (Base ⨯ Tree Base) → Tree Base @ Tree Base [(t^1) |-> 5/2, (2) |-> 3]*)
delete a t = match ~ splay a t with
  | leaf -> leaf
  | node l b r → if a == b
    then match l with
      | leaf → r
      | l    → match ~ splay_max l with
        | leaf -> leaf
        | node ll m d_ → node ll m r
    else node l b r

{-# STRONG_CF #-}
insert ∷ (Base ⨯ Tree Base) → Tree Base | Tree Base [t ↦ 1/2, (2) ↦ 1/2, (t^1) ↦ 2] → Tree Base [e1 ↦ 1/2]
(* {Tree Base [(t^1,1) ↦ 1] → Tree Base [(e1^1) ↦ 1]} *)
(* insert ∷ (Base ⨯ Tree Base) → Tree Base @ Tree Base [(t^1) |-> 2, (2) |-> 1]*)
insert a t = match splay a t with
  | leaf → node leaf a leaf
  | node l b r → if a == b
    then node l a r
    else if a < b
      then node l a (node leaf b r)
    else node (node l b leaf) a r


(* fromList :: (List Base * Tree Base) -> Tree Base | List Base [(l^(1,1)) ↦ 2, (l^(1,0),2) |-> 1], Tree Base [t ↦ 1/2, (2) ↦ 1] → Tree Base [e1 ↦ 1/2, (2) |-> 1 ] { List Base [(l^(0,1)) ↦ 2, (2) |-> 1] → Tree Base [(e1^1) ↦ 2, (2) |-> 1]}*)
(* fromList :: (List Base * Tree Base) -> Tree Base @ List Base [(l^(1,1)) |-> 2, (l^(1,0),2) |-> 1] *)


{-# STRONG_CF #-}
fromList :: (List Base * Tree Base) -> Tree Base | List Base [(l^(1,0),2) |-> 1/2, (l^(1,1)) |-> 2], Tree Base [t ↦ 1/2] → Tree Base [e1 ↦ 1/2] (*{ List Base [(l^(0,1)) ↦ 1, (2) |-> 1/4] → Tree Base [(e1^1) ↦ 1, (2) |-> 1/4]}*)
fromList l t = match t with
  | node x1 _ x2 -> error
  | leaf -> match l with 
    | [] -> leaf
    | cons x xs -> insert x (fromList xs leaf)
  



(*
 * contains ∷ Ord α ⇒ (α ⨯ Tree α) → Bool
 * contains a t = match t with
 *  | leaf → false
 *  | t    → match splay a t with
 *    | leaf       → false
 *    | node _ b _ → (a == b)
*)
