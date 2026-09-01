let ft_abs x = if x < 0. then -.x else x
let ft_sq x = x *. x

let ft_sqrt x =
  assert (x >= 0.);
  if x = 0. then 0.
  else
    let delta = 0.00000001 in
    let rec ft_sqrt' ?(y = x /. 2.) () =
      let y' = (y +. (x /. y)) /. 2. in
      if ft_abs (y' -. y) < delta then y' else ft_sqrt' ~y:y' ()
    in

    ft_sqrt' ()

let eu_dist a b =
  assert (Array.length a = Array.length b);

  let rec eu_dist' ?(i = 0) a b =
    if i = Array.length a then 0.
    else ft_sq (a.(i) -. b.(i)) +. eu_dist' ~i:(i + 1) a b
  in

  ft_sqrt (eu_dist' a b)

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
