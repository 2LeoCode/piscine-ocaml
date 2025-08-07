open Ex04.Fibonacci
open Printf

let ( %> ) f g x = x |> f |> g
let print_case = fibonacci %> printf "%d\n"

let () =
  -42 |> print_case;
  print_case 1;
  print_case 3;
  print_case 6
