let ft_rot_n n s =
  assert (n >= 0);

  let rot_char (min, max) n c =
    assert (max >= min && c >= min && c <= max);
    let diff = int_of_char max - int_of_char c in
    char_of_int
      (if n > diff then int_of_char min + n - diff - 1 else int_of_char c + n)
  in

  let rot_lowercase_char = rot_char ('a', 'z') in

  let rot_uppercase_char = rot_char ('A', 'Z') in

  let n' = n mod 26 in
  String.map
    (fun c ->
      match c with
      | 'a' .. 'z' -> rot_lowercase_char n' c
      | 'A' .. 'Z' -> rot_uppercase_char n' c
      | c -> c)
    s
