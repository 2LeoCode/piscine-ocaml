open Ex07.Ft_is_palindrome
open Printf

let ( %> ) f g x = x |> f |> g
let print_case = ft_is_palindrome %> printf "%b\n"

let () =
  print_case "radar";
  print_case "madam";
  print_case "car";
  print_case "anna";
  print_case ""
