let repeat_string ?(str = "x") n =
  let rec repeat_string' s acc = function
    | 0 -> acc
    | n -> (repeat_string' [@tailcall]) s (s ^ acc) (n - 1)
  in

  if n < 0 then "Error" else repeat_string' str "" n

let () =
  let print_case ?str n = repeat_string ?str n |> print_endline in

  print_case (-1);
  print_case 0;
  print_case ~str:"Toto" 1;
  print_case 2;
  print_case ~str:"a" 5;
  print_case ~str:"what" 3
