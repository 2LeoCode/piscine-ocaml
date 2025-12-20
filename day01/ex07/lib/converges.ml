let rec converges f x n =
  if n < 0 then false
  else
    let x' = f x in
    x' = x || (n <> 0 && (converges [@tailcall]) f x' (n - 1))

let () =
  let print_case f start n = converges f start n |> Printf.printf "%b\n" in

  print_case (( + ) 2) 2 (-42);
  print_case (( * ) 2) 2 5;
  print_case (fun x -> x / 2) 2 3;
  print_case (fun x -> x / 2) 2 2
