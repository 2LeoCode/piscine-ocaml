let ft_print_rev s =
  let rec ft_print_rev' i =
    if i <> -1 then (
      print_char s.[i];
      (ft_print_rev' [@tailcall]) (i - 1))
  in
  ft_print_rev' (String.length s - 1);
  print_char '\n'
