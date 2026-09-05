let () =
  let amy = new Ex01.People.people "amy" in
  let matt = new Ex01.Doctor.doctor "matt" amy in

  print_endline matt#to_string;
  matt#talk;
  matt#travel_in_time 0 249;
  print_endline matt#to_string;
  matt#travel_in_time 249 178;
  print_endline matt#to_string;
  matt#use_sonic_screwdriver
