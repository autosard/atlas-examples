moveToFront :: (List a * List a) -> (List a * List a) @> (r^1) |-> 1
moveToFront f r = match r with
  | [] -> (f, [])
  | cons x r -> ~ moveToFront (cons x f) r

snoc :: (List a * List a * a) -> (List a * List a)
snoc f r x = match f with
  | [] -> (cons x [], [])
  | f -> (f, cons x r)

tail :: (List a * List a) -> (List a * List a) @ () |-> 1
(* | [[() |-> 1, (r^1) |-> 1] -> [(e2^1) |-> 1]] *)
tail f r = match f with
  | [] -> ([], []) (* error case *)
  | cons x ff -> match ff with
    | [] -> ~ moveToFront [] r
    | ff -> ~ (ff, r)

