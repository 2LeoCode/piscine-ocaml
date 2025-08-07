let ft_print_alphabet () =
  let rec ft_print_alphabet' c =
    print_char c;
    if c < 'z' then
      (ft_print_alphabet' [@tailcall]) (char_of_int (int_of_char c + 1))
    else print_newline ()
  in
  ft_print_alphabet' 'a'
