let ft_is_palindrome s =
  let len = String.length s in
  let rec ft_is_palindrome' i =
    if i <> len / 2 then
      if s.[i] <> s.[len - i - 1] then false
      else (ft_is_palindrome' [@tailcall]) (i + 1)
    else true
  in
  ft_is_palindrome' 0
