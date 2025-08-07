let fibonacci = function
  | n when n < 0 -> -1
  | 0 -> 0
  | 1 -> 1
  | n ->
      let rec fibonacci' a b = function
        | i when i = n -> a + b
        | i -> (fibonacci' [@tailcall]) b (a + b) (i + 1)
      in
      fibonacci' 0 1 2
