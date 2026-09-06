let jokes = ()

let () =
  if Array.length Sys.argv <> 2 then
    print_endline ("Usage: " ^ Sys.argv.(0) ^ " <joke-file-path>")
  else (
    Random.self_init ();
    let jokes =
      try ref (In_channel.with_open_text Sys.argv.(1) In_channel.input_lines)
      with Sys_error e ->
        print_endline ("Error: Failed to read jokes file: " ^ e);
        exit 1
    in

    let should_quit = ref false in
    while not !should_quit do
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
            print_endline
              (match int_of_string (read_line ()) with
              | exception Failure _ -> "Invalid index: not a number"
              | idx when idx < 0 || idx >= List.length !jokes ->
                  "Invalid index: out of range"
              | idx ->
                  jokes := List.filteri (fun i _ -> i <> idx) !jokes;
                  "Jokes sucesfully deleted !"))
      | "tell" ->
          if List.length !jokes = 0 then print_endline "No joke to tell"
          else print_endline (List.nth !jokes (Random.int (List.length !jokes)))
      | "save" ->
          print_endline
            (match
               Out_channel.with_open_text Sys.argv.(1) (fun oc ->
                   List.iter
                     (fun joke -> Out_channel.output_string oc (joke ^ "\n"))
                     !jokes)
             with
            | exception Sys_error msg -> "Failed to save jokes: " ^ msg
            | () -> "Jokes succesfully saved !")
      | "quit" -> should_quit := true
      | "help" ->
          print_endline
            "add: add a joke\n\
             del: delete a joke\n\
             tell: randomly pick a saved joke and print it\n\
             save: update jokes file with changes\n\
             help: show this help message\n\
             quit: quit the program"
      | "" -> ()
      | cmd ->
          print_endline
            ("Unknown command: '" ^ cmd
           ^ "', type `help` to see available commands.")
    done)
