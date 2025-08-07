open Ex07.Converges
open Printf

let ( %> ) f g x = x |> f |> g
let print_case f start = converges f start %> printf "%b\n"

let () =
  print_case (( * ) 2) 2 5;
  print_case (fun x -> x / 2) 2 3;
  print_case (fun x -> x / 2) 2 2
