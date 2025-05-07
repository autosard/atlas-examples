go :: (Num * List Base * List Base) -> (List Base * List Base) | List Base [(l^1) |-> 1] -> List Base [(e1^1) |-> 1]
go k l r = match l with
  | [] -> ([], r)
  | cons x xs -> if k > 1
    then ~ go (k - 1) xs (cons x r) 
    else (xs, (cons x r))

pop :: (Num * List Base) -> (List Base * List Base) | List Base [(l^1) |-> 1] -> List Base [(e1^1) |-> 1]
pop k l = go k [] l

push :: (Base * List Base) -> (List Base * List Base) | List Base [(l^1) |-> 1, () |-> 1] -> List Base [(e1^1) |-> 1]
push a l = (cons a l, [])



      


