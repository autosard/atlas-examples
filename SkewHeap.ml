{-# POTENTIAL (Tree Num: loglrx) #-}
{-# RHSTERMS #-}

min :: Tree Num -> Num | Tree Num [] -> Tree Num []
min x = match x with
  | leaf -> error
  | node t a u -> a

insert ∷ (Num ⨯ Tree Num) → Tree Num | Tree Num [(e1^1) |-> 1/2, (x^1) ↦ 1, x ↦ 1, (2) |-> 3] → Tree Num [e1 ↦ 1, (2) |-> 1]
insert a x = (meld (node leaf a leaf) x)

{-# STRONGCF #-}
delete_min ∷ Tree Num → Tree Num | Tree Num [(e1^1) ↦ 1/2, (x^1) |-> 1, x ↦ 1, (2) |-> 1] → Tree Num [e1 ↦ 1, (2) |-> 1]
delete_min x = match x with
  | leaf       → leaf
  | node t a u → meld t u


(*meld ∷ (Tree Num ⨯ Tree Num) → Tree Num | Tree Num [x ↦ 1, y ↦ 1, (e1^1) ↦ 1/2, (x^1) |-> 1/2, (y^1) |-> 1/2] → Tree Num [e1 ↦ 1, (2) |-> 1] {Tree Num [] -> Tree Num []}*)
meld ∷ (Tree Num ⨯ Tree Num) → Tree Num | Tree Num [x ↦ 1, y ↦ 1, (e1^1) ↦ 105/163, (x^1) |-> 3115/7824, (y^1) |-> 3115/7824] → Tree Num [e1 ↦ 1] 
meld x y = match x with
  | leaf          → y
  | node t a u → match y with
    | leaf             → (node t a u)
    | node v b w → if a <= b
      then bal t a (~ meld (node v b w) u)
      else bal v b (~ meld (node t a u) w)


bal :: (Tree Num  * Num * Tree Num) -> Tree Num | Tree Num [t ↦ 1, u ↦ 1, (t^1,u^1) ↦ 105/163, (u^1) |-> -105/163] → Tree Num [e1 ↦ 1] 
bal t a u = node u a t

(*
sort :: List Num -> List Num
sort l = match l with
  | [] -> []
  | l -> let h = inserts l leaf in
           removes h []
     
inserts :: (List Num * Tree Num) -> Tree Num
inserts l h = match l with
  | [] -> h
  | cons x xs -> inserts xs (insert x h)

removes :: (Tree Num * List Num) -> List Num
removes h l = match h with
  | leaf -> l
  | h    -> removes (delete_min h) (cons (min h) l)
*)
