let fibonacci = function
  | n when n < 0 -> -1
  | 0 -> 0
  | 1 -> 1
  | n ->
      let rec fibonacci' a b = function
        | i when i = n -> a + b
        | i -> (fibonacci' [@tailcall]) b (a + b) (i + 1)
      in
      fibonacci' 0 1 2

let () =
  let print_case x = x |> fibonacci |> Printf.printf "%d\n" in

  print_case (-42);
  print_case 1;
  print_case 3;
  print_case 6
