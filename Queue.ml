(**
 * A version of a purely functional queue, represented by a front and a rear list.
 *)

(* moveToFront :: (List a * List a) -> (List a * List a) | [[(r^1) |-> 1] -> [(e2^1) |-> 1]]*)
moveToFront :: (List Base * List Base) -> (List Base * List Base) @ List Base [(r^1) |-> 0]
moveToFront f r = match r with
  | [] -> (f, [])
  | cons x xs -> moveToFront (~ cons x f) xs

(* snoc :: (List Base * List Base * a) -> (List Base * List Base) | [[() |-> 2, (r^1) |-> 1] -> [(e2^1) |-> 1]] *)
snoc :: (List Base * List Base * a) -> (List Base * List Base) @ List Base [() |-> 2]
snoc f r x = match f with
  | [] -> ~ (cons x [], [])
  | f -> ~ (f, cons x r)

head :: (List Base * List Base) -> a @> List Base [() |-> 1]
head f r = match f with
  | [] -> ~ error
  | cons x xs -> ~ x

(* tail :: (List Base * List Base) -> (List Base * List Base) | [[() |-> 1, (r^1) |-> 1] -> [(e2^1) |-> 1]] *)
tail :: (List Base * List Base) -> (List Base * List Base) @ List Base [() |-> 1]
tail f r = match f with
  | [] -> ~ error
  | cons x xs -> ~ match xs with
    | [] -> moveToFront [] r
    | xs -> (xs, r)
