class people (name : string) =
  object
    val name = name
    val mutable hp = 100
    initializer print_endline (name ^ " has been born")

    method to_string =
      "people with name = " ^ name ^ ", hp = " ^ string_of_int hp

    method talk = print_endline ("I’m " ^ name ^ "! Do you know the Doctor?")
    method die = print_endline "Aaaarghh!"
  end
