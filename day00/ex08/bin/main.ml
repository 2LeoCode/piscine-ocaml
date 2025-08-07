open Ex08.Ft_rot_n

let ( %> ) f g x = x |> f |> g
let print_case n = ft_rot_n n %> print_endline

let () =
  print_case 1 "abcdefghijklmnopqrstuvwxyz";
  print_case 13 "abcdefghijklmnopqrstuvwxyz";
  print_case 42 "0123456789";
  print_case 2 "OI2EAS67B9";
  print_case 0 "Damned !";
  print_case 42 "";
  print_case 1 "NBzlk qnbjr !"
