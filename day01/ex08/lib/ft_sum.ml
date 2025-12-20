let ft_sum f low high =
  if low > high then nan
  else
    let rec ft_sum' acc i =
      if i > high then acc else (ft_sum' [@tailcall]) (acc +. f i) (i + 1)
    in
    ft_sum' 0. low

let () =
  let print_case f low high = ft_sum f low high |> Printf.printf "%f\n" in
  let sq i = float_of_int (i * i) in

  print_case sq 1 10;
  print_case sq 42 1
