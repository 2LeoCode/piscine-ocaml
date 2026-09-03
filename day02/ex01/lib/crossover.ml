let[@tail_mod_cons] rec crossover l1 l2 =
  let rec list_has v = function
    | [] -> false
    | head :: rest -> head = v || (list_has [@tailcall]) v rest
  in

  match l2 with
  | [] -> []
  | head :: rest when list_has head l1 ->
      head :: (crossover [@tailcall]) l1 rest
  | _ :: rest -> (crossover [@tailcall]) l1 rest

let () =
  let print_int_list l =
    print_string "[ ";
    List.iter (Printf.printf "%d ") l;
    print_endline "]"
  in
  let print_case a b = crossover a b |> print_int_list in

  print_case [ 0; 1; 2; 3; 4; 5; 6 ] [ 10; 9; 8; 7; 6; 5 ]
