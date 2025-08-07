let rec iter f x = function
  | n when n < 0 -> -1
  | 0 -> x
  | n -> (iter [@tailcall]) f (f x) (n - 1)
