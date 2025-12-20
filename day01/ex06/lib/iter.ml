let iter f x n =
  if n < 0 then -1
  else
    let rec iter' x = function
      | 0 -> x
      | n -> (iter' [@tailcall]) (f x) (n - 1)
    in
    iter' x n

let () =
  let print_case f start n = iter f start n |> Printf.printf "%d\n" in

  print_case (fun x -> x) 300 (-42);
  print_case (fun x -> x * x) 2 4;
  print_case (fun x -> x * 2) 2 4
