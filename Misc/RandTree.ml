descend ∷ Tree Base → Tree Base | Tree Base [(t^1) ↦ 1] → Tree Base []
descend t = match t with
  | leaf       → leaf
  | node l a r → if coin
    then node (~ descend l) a r
    else node l a (~ descend r)
