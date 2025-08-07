let _encode =
  let[@tail_mod_cons] rec encode' n = function
    | [] -> []
    | head :: next :: rest when head = next ->
        next :: rest |> (encode' [@tailcall]) (n + 1)
    | head :: rest -> head :: n :: (encode' [@tailcall]) 1 rest
  in
  encode' 1

let _string_of_encoded =
  let rec string_of_encoded' acc = function
    | [] -> acc
    | head :: rest -> string_of_encoded' (string_of_int head ^ acc) rest
  in
  string_of_encoded' ""

let sequence = function
  | n when n < 0 -> ""
  | n ->
      let rec sequence' prev = function
        | 0 -> prev
        | i -> sequence' (_encode prev) (i - 1)
      in
      sequence' [ 1 ] n |> _string_of_encoded
