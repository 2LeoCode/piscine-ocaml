let ft_print_alphabet () =
  let rec ft_print_alphabet' c =
    print_char c;
    if c < 'z' then
      (c |> int_of_char) + 1 |> char_of_int |> (ft_print_alphabet' [@tailcall])
    else print_char '\n'
  in
  ft_print_alphabet' 'a'
