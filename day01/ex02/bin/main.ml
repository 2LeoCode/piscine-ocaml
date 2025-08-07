open Ex02.Ackermann
open Printf

let ( %> ) f g x = x |> f |> g
let print_case a = ackermann a %> printf "%d\n"

let () =
  print_case (-1) 7;
  print_case 0 0;
  print_case 2 3;
  print_case 4 1
