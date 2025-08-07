open Ex06.Iter
open Printf

let ( %> ) f g x = x |> f |> g
let print_case f start = iter f start %> printf "%d\n"

let () =
  print_case (fun x -> x * x) 2 4;
  print_case (fun x -> x * 2) 2 4
