let rec tak x y z =
  if y < x then
    (tak [@tailcall]) (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)
  else z

let () =
  let print_case a b c = tak a b c |> Printf.printf "%d\n" in

  print_case 1 2 3;
  print_case 5 23 7;
  print_case 9 1 0;
  print_case 1 1 1;
  print_case 0 42 0;
  print_case 23498 98734 98776
