class ['a] army =
  object
    val mutable members : 'a list = []
    method add member = members <- member :: members

    method delete =
      members <-
        (match members with
        | [] -> invalid_arg "There is not a single member left"
        | _ :: rest -> rest)
  end
