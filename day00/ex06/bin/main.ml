open Ex06.Ft_string_all
open Printf

let ( %> ) f g x = x |> f |> g
let is_digit c = c >= '0' && c <= '9'
let print_case = ft_string_all is_digit %> printf "%b\n"

let () =
  print_case "0123456789";
  print_case "O12EAS67B9"
