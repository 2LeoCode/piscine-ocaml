let ft_string_all pred s =
  let len = String.length s in
  let rec ft_string_all' i =
    if i == len then true else pred s.[i] && (ft_string_all' [@tailcall]) (i + 1)
  in
  ft_string_all' 0
