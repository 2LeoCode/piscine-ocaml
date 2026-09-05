let () =
  let peter = new Ex00.People.people "peter" in
  let lisa = new Ex00.People.people "lisa" in

  peter#talk;
  lisa#die;
  print_endline peter#to_string;
  print_endline lisa#to_string
