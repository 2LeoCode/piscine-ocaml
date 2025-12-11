let ft_is_palindrome s =
  let len = String.length s in
  let rec ft_is_palindrome' i =
    i = len / 2
    || (s.[i] = s.[len - i - 1] && (ft_is_palindrome' [@tailcall]) (i + 1))
  in
  ft_is_palindrome' 0
