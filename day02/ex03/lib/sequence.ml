let sequence n =
  let encode =
    let[@tail_mod_cons] rec encode' n = function
      | [] -> []
      | head :: next :: rest when head = next ->
          (encode' [@tailcall]) (n + 1) (next :: rest)
      | head :: rest -> head :: n :: (encode' [@tailcall]) 1 rest
    in
    encode' 1
  in

  let string_of_encoded =
    let rec string_of_encoded' acc = function
      | [] -> acc
      | head :: rest -> string_of_encoded' (string_of_int head ^ acc) rest
    in
    string_of_encoded' ""
  in

  if n < 0 then ""
  else
    (* let rec sequence' prev = function *)
    (*   | 0 -> prev *)
    (*   | i -> sequence' (encode prev) (i - 1) *)
    (* in *)
    (* sequence' [ 1 ] n |> string_of_encoded *)
    let rec sequence' prev = function
      | 0 -> prev
      | n ->
        let rec sequence'' = () in

        sequence'':Wq 
    in

    sequence' "1" n

let () =
  let print_case x = x |> sequence |> print_endline in

  print_case 0;
  print_case 1;
  print_case 2;
  print_case 3;
  print_case 4;
  print_case 5;
  print_case 6;
  print_case 7;
  print_case 8;
  print_case 9
