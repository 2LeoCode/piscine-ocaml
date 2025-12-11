let _pi = 4. *. atan 1.

let leibniz_pi delta =
  let abs_float n = if n < 0. then -.n else n in

  let rec leibniz_pi' i x =
    if abs_float ((4. *. x) -. _pi) < delta then i
    else
      (leibniz_pi' [@tailcall]) (i + 1)
        (x +. ((-1. ** float_of_int i) /. float_of_int ((i * 2) + 1)))
  in
  leibniz_pi' 0 0.
