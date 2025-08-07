let rec ft_countdown x =
  if x <= 0 then print_endline "0"
  else (
    print_int x;
    print_newline ();
    (ft_countdown [@tailcall]) (x - 1))
