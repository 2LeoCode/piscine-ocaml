open Ex01.Crossover
open Printf

let ( %> ) f g x = x |> f |> g

let print_int_list l =
  print_string "[ ";
  List.iter (printf "%d ") l;
  print_endline "]"

let print_case a = crossover a %> print_int_list
let () = print_case [ 0; 1; 2; 3; 4; 5; 6 ] [ 10; 9; 8; 7; 6; 5 ]
