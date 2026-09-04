let my_sleep () = Unix.sleep 1

let my_better_sleep n =
  for _ = 1 to n do
    my_sleep ()
  done

let unreachable () = assert false

let main () =
  if Array.length Sys.argv == 2 then
    int_of_string Sys.argv.(1) |> my_better_sleep

let () = main ()
