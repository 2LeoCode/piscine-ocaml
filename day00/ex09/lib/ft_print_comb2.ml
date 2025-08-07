let _print_comb_item x =
  if x < 10 then print_char '0';
  print_int x

let ft_print_comb2 () =
  let[@tail_mod_cons] rec ft_print_comb2' a b =
    _print_comb_item a;
    print_char ' ';
    _print_comb_item b;
    if a <> 98 then (
      print_string ", ";
      (ft_print_comb2' [@tailcall])
        (if b = 99 then a + 1 else a)
        (if b = 99 then a + 2 else b + 1))
    else print_newline ()
  in
  ft_print_comb2' 0 1
