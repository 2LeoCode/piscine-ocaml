let main () =
  if Array.length Sys.argv == 2 then Unix.sleep (int_of_string Sys.argv.(1))

let () = main ()
