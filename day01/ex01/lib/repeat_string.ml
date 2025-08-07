let repeat_string ?(str = "x") =
  let rec repeat_string' s acc = function
    | n when n < 0 -> "Error"
    | 0 -> acc
    | n -> (repeat_string' [@tailcall]) s (s ^ acc) (n - 1)
  in
  repeat_string' str ""
