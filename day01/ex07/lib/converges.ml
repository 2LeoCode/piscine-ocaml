let rec converges f x n =
  let x' = f x in
  x' = x || (n <> 0 && (converges [@tailcall]) f x' (n - 1))
