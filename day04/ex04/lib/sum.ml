let sum a b = a +. b

let () =
  let rec fib = function
    | n when n < 2 -> float_of_int n
    | n -> sum (fib (n - 1)) (fib (n - 2))
  in

  for i = 0 to 20 do
    print_float (fib i);
    print_newline ()
  done
