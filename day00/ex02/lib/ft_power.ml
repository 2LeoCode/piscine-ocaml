let ft_power x y =
  assert (x >= 0 && y >= 0);
  let rec ft_power' acc = function
    | 0 -> 1
    | 1 -> acc
    | y' -> (ft_power' [@tailcall]) (acc * x) (y' - 1)
  in
  ft_power' x y
