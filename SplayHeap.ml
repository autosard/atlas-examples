(**
 * The function definitions in this file are taken from or made to match
 * Section 7 of
 *
 *   Tobias Nipkow, Hauke Brinkop
 *   Amortized Complexity Verified
 *   Journal of Automated Reasoning, Vol. 62, Iss. 3, pp. 367-391
 *   https://doi.org/10.1007/s10817-018-9459-3
 *)

(*

Version with literal numbers.

insert x h = match partition x h with
  | leaf      → leaf
  | (l, y, r) → (l, x, r)

partition p t = match t with
  | leaf           → (leaf, 0, leaf)
  | (tab, ab, tbc) → if ab <= p
    then match tbc with
      | leaf        → ((tab, ab, tbc), 0, leaf)
      | (tb, b, tc) → if b <= p
        then match partition p tc with
          | leaf          → leaf
          | (tc1, x, tc2) → (((tab, ab, tb), b, tc1), 0, tc2)
        else match partition p tb with
          | leaf          → leaf
          | (tb1, x, tb2) → ((tab ab tb1), 0, (tb2, b, tc))
    else match tab with
      | leaf        → (leaf, 0, (tab, ab, tbc))
      | (ta, a, tb) → if a <= p
        then match partition p tb with
          | leaf          → leaf
          | (tb1, x, tb2) → ((ta, a, tb1), 0, (tb2, ab, tbc))
        else match partition p ta with
          | leaf          → leaf
          | (ta1, x, ta2) → (ta1, 0, (ta2, a, (tb, ab, tbc)))
*)

insert ∷ Ord α ⇒ α ⨯ α ⨯ Tree α | [ 0 ↦ 1, (1 0) ↦ 6, (0 2) ↦ 1 ] → Tree α | [ 0 ↦ 1, (0 2) ↦ 1 ]
insert d x h = match partition d x h with
  | leaf      → (leaf, x, leaf)
  | (l, y, r) → (l, x, r)


partition ∷ Ord α ⇒ α ⨯ α ⨯ Tree α | [ 0 ↦ 1, (1 0) ↦ 3, (0 2) ↦ 2 ] → Tree α | [ 0 ↦ 1, (0 2) ↦ 1 ]
partition d p t = match t with
  | leaf           → leaf
  | (tab, ab, tbc) → if ab <= p
    then match tbc with
      | leaf → ((tab, ab, leaf), d, leaf)
      | (tb, b, tc) → if b <= p
        then match partition d p tc with
          | leaf          → leaf
          | (tc1, x, tc2) → (((tab, ab, tb), b, tc1), d, tc2) (* zag zag *)
        else match partition d p tb with
          | leaf          → leaf
          | (tb1, x, tb2) → ((tab, ab, tb1), d, (tb2, b, tc)) (* zag zig *)
    else match tab with
      | leaf → (leaf, d, (leaf, ab, tbc))
      | (ta, a, tb) → if a <= p
        then match partition d p tb with
          | leaf          → leaf
          | (tb1, x, tb2) → ((ta, a, tb1), d, (tb2, ab, tbc)) (* zig zag *)
        else match partition d p ta with
          | leaf          → leaf
          | (ta1, x, ta2) → (ta1, d, (ta2, a, (tb, ab, tbc))) (* zig zig *)

del_min ∷ (Eq (Tree α)) ⇒ Tree α | [ 0 ↦ 1, (1 0) ↦ 1, (1 1) ↦ 1, (0 2) ↦ 1 ] → Tree α | [ 0 ↦ 1, (0 2) ↦ 1 ]
del_min t = match t with
  | leaf          → leaf
  | (tab, b, tc) → match tab with
    | leaf → tc
    | (ta, a, tb) → if ta == leaf
      then (tb, b, tc)
      else (del_min ta, a, (tb, b, tc))