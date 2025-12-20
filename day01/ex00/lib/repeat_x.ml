let repeat_x n =
  let rec repeat_x' acc = function
    | 0 -> acc
    | n -> (repeat_x' [@tailcall]) ("x" ^ acc) (n - 1)
  in

  if n < 0 then "Error" else repeat_x' "" n

let () =
  let print_case x = x |> repeat_x |> print_endline in

  print_case (-1);
  print_case 0;
  print_case 1;
  print_case 2;
  print_case 5;
  print_case 17;
  print_case 48;
  print_case 72
