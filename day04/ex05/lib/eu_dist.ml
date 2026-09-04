let sq x = x *. x

let eu_dist a b =
  assert (Array.length a = Array.length b);

  let rec eu_dist' ?(i = 0) a b =
    if i = Array.length a then 0.
    else sq (a.(i) -. b.(i)) +. eu_dist' ~i:(i + 1) a b
  in

  sqrt (eu_dist' a b)

let () =
  print_float
    (eu_dist [| 72.; 14.; 5.; 89.; 33. |] [| 21.; 66.; 40.; 7.; 94. |]);
  print_newline ();
  print_float
    (eu_dist [| 58.; 3.; 82.; 11.; 50. |] [| 9.; 75.; 26.; 63.; 18. |]);
  print_newline ();
  print_float
    (eu_dist [| 44.; 91.; 37.; 2.; 79. |] [| 12.; 55.; 6.; 98.; 31. |]);
  print_newline ()
