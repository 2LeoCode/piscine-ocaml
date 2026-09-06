let my_sleep () = Unix.sleep 1

let () =
  let my_better_sleep n =
    for _ = 1 to n do
      my_sleep ()
    done
  in

  if Array.length Sys.argv == 2 then
    match int_of_string Sys.argv.(1) with
    | exception _ -> print_endline "Error: not a number"
    | n when n < 0 -> print_endline "Error: negative number"
    | n -> my_better_sleep n
