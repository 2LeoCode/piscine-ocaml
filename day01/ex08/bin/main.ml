open Ex08.Ft_sum
open Printf

let ( %> ) f g x = x |> f |> g
let print_case f start = ft_sum f start %> printf "%f\n"
let sq i = float_of_int (i * i)

let () =
  print_case sq 1 10;
  print_case sq 42 1
