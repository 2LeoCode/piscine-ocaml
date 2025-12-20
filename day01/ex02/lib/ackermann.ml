let rec ackermann m n =
  match (m, n) with
  | m, n when m < 0 || n < 0 -> -1
  | 0, n -> n + 1
  | m, 0 -> (ackermann [@tailcall]) (m - 1) 1
  | m, n -> (ackermann [@tailcall]) (m - 1) (ackermann m (n - 1))

let () =
  let print_case a b = ackermann a b |> Printf.printf "%d\n" in

  print_case (-1) 7;
  print_case 0 0;
  print_case 2 3;
  print_case 4 1
