let ft_print_comb () =
  let rec ft_print_comb' a b c =
    print_int a;
    print_int b;
    print_int c;
    if a <> 7 then (
      print_string ", ";
      (ft_print_comb' [@tailcall])
        (if b = 8 then a + 1 else a)
        (if b = 8 then a + 2 else if c = 9 then b + 1 else b)
        (if c = 9 then if b = 8 then a + 3 else b + 2 else c + 1))
    else print_string "\n"
  in
  ft_print_comb' 0 1 2
