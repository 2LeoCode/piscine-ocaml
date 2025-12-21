let[@tail_mod_cons] rec crossover l =
  let rec list_contains v = function
    | [] -> false
    | head :: rest -> head = v || (list_contains [@tailcall]) v rest
  in

  function
  | [] -> []
  | head :: rest when list_contains head l ->
      head :: (crossover [@tailcall]) l rest
  | _ :: rest -> (crossover [@tailcall]) l rest

let () =
  let print_int_list l =
    print_string "[ ";
    List.iter (Printf.printf "%d ") l;
    print_endline "]"
  in
  let print_case a b = crossover a b |> print_int_list in

  print_case [ 0; 1; 2; 3; 4; 5; 6 ] [ 10; 9; 8; 7; 6; 5 ]
