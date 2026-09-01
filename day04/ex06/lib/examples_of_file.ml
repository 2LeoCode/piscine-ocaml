let examples_of_file path =
  if not (String.ends_with ~suffix:".csv" path) then
    raise (Invalid_argument "invalid csv file path");

  let lines = In_channel.with_open_text path In_channel.input_lines in
  List.map
    (fun line ->
      let cols = String.split_on_char ',' line in
      match List.rev cols with
      | kind :: data ->
          if String.length kind <> 1 then raise Parsing.Parse_error
          else
            ( Array.of_list (List.rev_map (fun x -> float_of_string x) data),
              kind )
      | [] -> raise Parsing.Parse_error)
    lines

let () =
  let print_elem e =
    let arr, kind = e in
    print_string "[ ";
    for i = 0 to Array.length arr - 1 do
      print_float arr.(i);
      print_char ' '
    done;
    print_string "] ";
    print_string kind;
    print_newline ()
  in
  if Array.length Sys.argv <> 2 then
    print_endline ("Usage: " ^ Sys.argv.(0) ^ " <path-to-csv-file>")
  else ignore (List.map print_elem (examples_of_file Sys.argv.(1)))
