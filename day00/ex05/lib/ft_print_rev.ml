let ( %> ) f g x = x |> f |> g

let _explode s =
  let[@tail_mod_cons] rec explode' = function
    | i when i < 0 -> []
    | i -> s.[i] :: (explode' [@tailcall]) (i - 1)
  in
  (s |> String.length) - 1 |> explode'

let _implode l =
  let len = List.length l in
  let result = Bytes.create len in
  let rec implode' i = function
    | [] -> ()
    | x :: rest ->
        Bytes.set result i x;
        (implode' [@tailcall]) (i + 1) rest
  in
  implode' 0 l;
  Bytes.to_string result

let[@tail_mod_cons] rec _rev_list = function
  | [] -> []
  | x :: rest -> x :: (_rev_list [@tailcall]) rest

let _rev_string = _explode %> _rev_list %> _implode
let ft_print_rev = _rev_string %> print_endline
