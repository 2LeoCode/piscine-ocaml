let ft_sum f low high =
  if low > high then nan
  else
    let rec ft_sum' acc i =
      if i > high then acc else (ft_sum' [@tailcall]) (acc +. f i) (i + 1)
    in
    ft_sum' 0. low
