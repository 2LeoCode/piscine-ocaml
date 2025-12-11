let repeat_x n =
  let rec repeat_x' acc = function
    | n when n >= 64 ->
        (repeat_x' [@tailcall])
          ("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
         ^ acc)
          (n - 64)
    | n when n >= 32 ->
        (repeat_x' [@tailcall])
          ("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ^ acc)
          (n - 32)
    | n when n >= 16 ->
        (repeat_x' [@tailcall]) ("xxxxxxxxxxxxxxxx" ^ acc) (n - 16)
    | n when n >= 8 -> (repeat_x' [@tailcall]) ("xxxxxxxx" ^ acc) (n - 8)
    | n when n >= 7 -> (repeat_x' [@tailcall]) ("xxxxxxx" ^ acc) (n - 7)
    | n when n >= 6 -> (repeat_x' [@tailcall]) ("xxxxxx" ^ acc) (n - 6)
    | n when n >= 5 -> (repeat_x' [@tailcall]) ("xxxxx" ^ acc) (n - 5)
    | n when n >= 4 -> (repeat_x' [@tailcall]) ("xxxx" ^ acc) (n - 4)
    | n when n >= 3 -> (repeat_x' [@tailcall]) ("xxx" ^ acc) (n - 3)
    | n when n >= 2 -> (repeat_x' [@tailcall]) ("xx" ^ acc) (n - 2)
    | 0 -> acc
    | n -> (repeat_x' [@tailcall]) ("x" ^ acc) (n - 1)
  in

  if n < 0 then "Error" else repeat_x' "" n
