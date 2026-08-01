balance :: (Tree Base ⨯ Base ⨯ Tree Base) → Tree Base
balance t a u = node t a u

insert ∷ (Base ⨯ Tree Base) → Tree Base 
insert d t = match t with
  | leaf       → node leaf d leaf
  | node l a r → if a < d
    then balance (~ insert d l) a r
    else balance l a (~ insert d r)

delete ∷ (Base ⨯ Tree Base) → Tree Base
delete k t = match t with
  | leaf -> leaf
  | node l a r → if k < a then balance (~ delete l k) a r
    else if k > a then balance l a (~ delete r k)
    else match l with
      | leaf → r
      | l    → match ~ delete_max l with
        | (ll, m) → balance ll m r

delete_max ∷ Tree Base → (Tree Base ⨯ Base)
delete_max t = match t with
  | leaf       → error
  | node cl c cr → match cr with
    | leaf         → (cl, c)
    | node bl b br → match br with
      | leaf → ((node cl c bl), b)
      | br   → match ~ delete_max br with
        | (t1, m) → match t1 with
          | leaf         → error
          | node al a ar → (node (node (node cl c bl) b al) a ar, m)
