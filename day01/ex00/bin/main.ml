open Ex00.Repeat_x

let ( %> ) f g x = x |> f |> g
let print_case = repeat_x %> print_endline

let () =
  -1 |> print_case;
  print_case 0;
  print_case 1;
  print_case 2;
  print_case 5;
  print_case 17;
  print_case 48;
  print_case 72
