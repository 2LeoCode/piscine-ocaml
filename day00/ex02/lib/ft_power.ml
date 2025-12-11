let ft_power x y =
  assert (x >= 0 && y >= 0 && (x <> 0 || y <> 0));
  let rec ft_power' acc = function
    | 1 -> acc
    | y' -> (ft_power' [@tailcall]) (acc * x) (y' - 1)
  in
  if y = 0 then 1 else ft_power' x y
