let _rot_char (min, max) n c =
  assert (max >= min && c >= min && c <= max);
  let diff = int_of_char max - int_of_char c in
  char_of_int
    (if n > diff then int_of_char min + n - diff - 1 else int_of_char c + n)

let _rot_lowercase_char = _rot_char ('a', 'z')
let _rot_uppercase_char = _rot_char ('A', 'Z')

let ft_rot_n n s =
  assert (n >= 0);
  let n' = n mod 26 in
  String.map
    (fun c ->
      match c with
      | 'a' .. 'z' -> _rot_uppercase_char n' c
      | 'A' .. 'Z' -> _rot_lowercase_char n' c
      | c -> c)
    s
