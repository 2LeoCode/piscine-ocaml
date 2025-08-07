let rec _list_contains v = function
  | [] -> false
  | head :: rest -> head = v || (_list_contains [@tailcall]) v rest

let[@tail_mod_cons] rec crossover l = function
  | [] -> []
  | head :: rest when _list_contains head l ->
      head :: (crossover [@tailcall]) l rest
  | _ :: rest -> (crossover [@tailcall]) l rest
