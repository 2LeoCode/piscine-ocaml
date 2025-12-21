let encode l =
  let[@tail_mod_cons] rec encode' n = function
    | [] -> []
    | head :: next :: rest when head = next ->
        (encode' [@tailcall]) (n + 1) (next :: rest)
    | head :: rest -> (n, head) :: (encode' [@tailcall]) 1 rest
  in
  encode' 1 l

let () =
  let print_list print_elem l =
    print_string "[ ";
    List.iter (fun (n, x) -> print_elem n x) l;
    print_endline "]"
  in

  let print_int_elem = Printf.printf "%d:%d " in
  let print_string_elem = Printf.printf "%d:%s " in
  let print_case print_elem l = l |> encode |> print_list print_elem in

  print_case print_int_elem [ 0; 0; 0; 1; 2; 2; 3; 4; 5; 6; 6; 6; 6; 7; 7; 8 ];
  print_case print_string_elem [ "a"; "a"; "a"; "b"; "c"; "c"; "c" ];
  print_case print_int_elem []
