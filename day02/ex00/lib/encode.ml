let encode l =
  let[@tail_mod_cons] rec encode' n = function
    | [] -> []
    | head :: next :: rest when head = next ->
        (encode' [@tailcall]) (n + 1) (next :: rest)
    | head :: rest -> (n, head) :: (encode' [@tailcall]) 1 rest
  in
  encode' 1 l
