let rec ft_countdown x =
  if x < 0 then (ft_countdown [@tailcall]) 0
  else (
    print_int x;
    print_char '\n';
    if x <> 0 then (ft_countdown [@tailcall]) (x - 1))
