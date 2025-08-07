open Ex03.Sequence
open Printf

let ( %> ) f g x = x |> f |> g
let print_case = sequence %> print_endline

let () =
  print_case 0;
  print_case 1;
  print_case 2;
  print_case 3;
  print_case 4;
  print_case 5;
  print_case 6;
  print_case 7;
  print_case 8;
  print_case 9
