open Ex03.Tak
open Printf

let ( %> ) f g x = x |> f |> g
let print_case a b = tak a b %> printf "%d\n"

let () =
  print_case 1 2 3;
  print_case 5 23 7;
  print_case 9 1 0;
  print_case 1 1 1;
  print_case 0 42 0;
  print_case 23498 98734 98776
