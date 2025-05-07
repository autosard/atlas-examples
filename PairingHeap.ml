(**
 * The function definitions in this file are taken from or made to match
 * Section 8 of
 *
 *   Tobias Nipkow, Hauke Brinkop
 *   Amortized Complexity Verified
 *   Journal of Automated Reasoning, Vol. 62, Iss. 3, pp. 367-391
 *   https://doi.org/10.1007/s10817-018-9459-3
 *   https://dblp.org/rec/journals/jar/NipkowB19
 *)
      

(*
 The following functions are inlined versions of the provided definitions. 
 *)

merge ∷ Tree Base → Tree Base | Tree Base [h ↦ 1/2, (h^1) |-> 1/2] → Tree Base [e1 ↦ 1/2]
merge h = match h with
  | node h1 _ h2 -> match h1 with
    | leaf        → h2
    | node lx x rx → match h2 with
      | leaf        → node lx x rx
      | node ly y ry → if x < y
        then node (node ly y lx) x leaf
        else node (node lx x ly) y leaf

insert ∷ (Base ⨯ Tree Base) → Tree Base | Tree Base [h ↦ 1/2, (h^1) |-> 1/2, (2) |-> 1/2] → Tree Base [e1 ↦ 1/2]
(* insert_isolated ∷ (Base ⨯ Tree Base) → Tree Base @> Tree Base []*)
insert x h = match h with
  | leaf        → node leaf x leaf
  | node ly y _ → if x < y
    then node (node ly y leaf) x leaf
    else node (node leaf x ly) y leaf


delete_min ∷ Tree Base → Tree Base | Tree Base [h ↦ 1/2, (2) ↦ 1/2, (h^1) ↦ 1] → Tree Base [e1 ↦ 1/2]
delete_min h = match h with
  | node l _ _ → let x = ~ merge_pairs l in x


merge_pairs ∷ Tree Base → Tree Base | Tree Base [h ↦ 1/2, (h^1) ↦ 3/2] → Tree Base [e1 ↦ 1/2]
merge_pairs h = match h with
  | node la a ra → match ra with
    | leaf         → node la a leaf
    | node lb b rb → match ~ merge_pairs rb with
      | leaf → if a < b
        then node (node lb b la) a leaf
        else node (node la a lb) b leaf
      | node lc c rc → if a < b
        then if a < c
          then node (node lc c (node lb b la)) a rc
          else node (node (node lb b la) a lc) c rc
        else if b < c
          then node (node lc c (node la a lb)) b rc
          else node (node (node la a lb) b lc) c rc


