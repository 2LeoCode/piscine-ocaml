let leibniz_pi delta =
  if delta < 0. then -1
  else
    let pi = 4. *. atan 1. in

    let abs_float n = if n < 0. then -.n else n in

    let rec leibniz_pi' i x =
      if abs_float ((4. *. x) -. pi) < delta then i
      else
        (leibniz_pi' [@tailcall]) (i + 1)
          (x +. ((-1. ** float_of_int i) /. float_of_int ((i * 2) + 1)))
    in
    leibniz_pi' 0 0.

let () =
  let print_case delta = delta |> leibniz_pi |> Printf.printf "%d\n" in

  print_case (-44.);
  print_case 1.;
  print_case 0.1;
  print_case 0.01;
  print_case 0.001;
  print_case 0.0001
