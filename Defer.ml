(**
 * Example for (tick:defer) by Florian Zuleger.
 *
 * Requires expert knowledge
 *
 *  ∀ t. rk(t) >= 1
 *
 * for weakening.
 *)
f ∷ (Base ⨯ Tree Base) → Tree Base | Tree Base [t |-> 1, (t^1) |-> 1, (2) |-> 4] -> Tree Base [t |-> 1, (2) |-> 1]
f x t = match t with
  | leaf       → leaf
  | node l y r → let fl = ~ f x l in let fr = ~ f x r in if x == y
    then fl
    else fr
