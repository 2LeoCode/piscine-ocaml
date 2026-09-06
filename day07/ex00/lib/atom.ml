class virtual atom (name : string) (symbol : string) (atomic_number : int) =
  object
    method name = name
    method symbol = symbol
    method atomic_number = atomic_number

    method to_string =
      Printf.sprintf "atom with name = %s, symbol = %s, atomic_number = %d" name
        symbol atomic_number

    method equals (other : atom) = atomic_number = other#atomic_number
  end
