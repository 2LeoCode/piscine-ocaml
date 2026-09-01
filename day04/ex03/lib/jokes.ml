let main () =
  if Array.length Sys.argv <> 2 then
    print_endline ("Usage: " ^ Sys.argv.(0) ^ " <joke-file-path>")
  else (
    Random.self_init ();
    let jokes =
      try ref (In_channel.with_open_text Sys.argv.(1) In_channel.input_lines)
      with Sys_error e -> Stdlib.failwith ("Failed to read jokes file: " ^ e)
    in

    while true do
      match try read_line () with End_of_file -> exit 0 with
      | "add" ->
          print_string "Enter the joke to add: ";
          let joke = read_line () in
          jokes := joke :: !jokes;
          print_endline "Joke successfully added !"
      | "del" ->
          if List.length !jokes = 0 then print_endline "No joke to delete"
          else (
            List.iteri
              (fun i joke ->
                print_endline ("[" ^ string_of_int i ^ "] " ^ joke))
              !jokes;
            print_string "Enter the index of the joke to delete: ";
            let idx = int_of_string (read_line ()) in
            if idx < 0 || idx >= List.length !jokes then
              print_endline "Invalid index: out of range"
            else (
              jokes := List.filteri (fun i _ -> i <> idx) !jokes;
              print_endline "Joke succesfully deleted !"))
      | "tell" ->
          if List.length !jokes = 0 then print_endline "No joke to tell"
          else print_endline (List.nth !jokes (Random.int (List.length !jokes)))
      | "save" -> (
          try
            Out_channel.with_open_text Sys.argv.(1) (fun oc ->
                List.iter
                  (fun joke -> Out_channel.output_string oc (joke ^ "\n"))
                  !jokes);
            print_endline "Jokes succesfully saved !"
          with Sys_error e -> print_endline ("Failed to save jokes: " ^ e))
      | cmd -> print_endline ("Unknown command: '" ^ cmd ^ "'")
    done)

let () = main ()
