open Ex09.Leibniz_pi
open Printf

let ( %> ) f g x = x |> f |> g
let print_case = leibniz_pi %> printf "%d\n"

let () =
  print_case 1.;
  print_case 0.1;
  print_case 0.01;
  print_case 0.001;
  print_case 0.0001
