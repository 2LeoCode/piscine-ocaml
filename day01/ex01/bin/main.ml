open Ex01.Repeat_string

let ( %> ) f g x = x |> f |> g
let print_case ?str = repeat_string ?str %> print_endline

let () =
  print_case (-1);
  print_case 0;
  print_case ~str:"Toto" 1;
  print_case 2;
  print_case ~str:"a" 5;
  print_case ~str:"what" 3
