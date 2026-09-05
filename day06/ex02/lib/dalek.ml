class dalek =
  let () = Random.self_init () in

  let dialogue_lines =
    [|
      "Explain! Explain!";
      "Exterminate! Exterminate!";
      "I obey!";
      "You are the Doctor! You are the enemy of the Daleks!";
    |]
  in
  object (self)
    val name =
      let random_char_string start n length =
        let rec random_char_string' acc = function
          | 0 -> acc
          | i ->
              random_char_string'
                (acc ^ String.make 1 (char_of_int (Random.int n + start)))
                (i - 1)
        in
        random_char_string' "" length
      in

      let random_uc_string = random_char_string 65 26 in
      let random_lc_string = random_char_string 97 26 in
      "Dalek" ^ random_uc_string 1 ^ random_lc_string 2

    val mutable hp = 100
    val mutable shield = true
    initializer print_endline ("Beware! " ^ name ^ " has joined the dalek army!")

    method to_string =
      "dalek with name = " ^ name ^ ", hp = " ^ string_of_int hp ^ ", shield = "
      ^ if shield then "true" else "false"

    method talk =
      let choice = Random.int 4 in
      print_endline dialogue_lines.(choice)

    method exterminate (p : People.people) =
      p#die;
      ignore (shield = not shield)

    method die = print_endline "Emergency Temporal Shift!"
  end
