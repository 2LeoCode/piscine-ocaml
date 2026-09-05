class people = Ex03.People.people
class doctor = Ex03.Doctor.doctor
class dalek = Ex03.Dalek.dalek
class ['a] army = ['a] Ex03.Army.army

let () =
  let people_army = new army in
  let doctor_army = new army in
  let dalek_army = new army in

  let donna = new people "donna" in
  let amy = new people "amy" in
  let clara = new people "clara" in

  people_army#add donna;
  people_army#add amy;
  people_army#add clara;

  doctor_army#add (new doctor "david" donna);
  doctor_army#add (new doctor "matt" amy);
  doctor_army#add (new doctor "peter" clara);

  dalek_army#add (new dalek);
  dalek_army#add (new dalek);
  dalek_army#add (new dalek);
  dalek_army#add (new dalek);
  dalek_army#add (new dalek);

  people_army#delete;
  people_army#delete;
  people_army#delete;

  doctor_army#delete;
  doctor_army#delete;
  doctor_army#delete;

  dalek_army#delete;
  dalek_army#delete;
  dalek_army#delete;
  dalek_army#delete;
  dalek_army#delete;

  print_endline
    (match people_army#delete with exception _ -> "OK" | () -> "KO");

  print_endline
    (match doctor_army#delete with exception _ -> "OK" | () -> "KO");

  print_endline
    (match dalek_army#delete with exception _ -> "OK" | () -> "KO")
