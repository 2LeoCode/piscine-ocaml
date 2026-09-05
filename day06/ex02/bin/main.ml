let () =
  let clara = new Ex02.People.people "clara" in
  let peter = new Ex02.Doctor.doctor "peter" clara in
  let dalek = new Ex02.Dalek.dalek in
  let dalek2 = new Ex02.Dalek.dalek in

  clara#talk;
  dalek#talk;
  dalek2#talk;
  peter#talk;
  peter#use_sonic_screwdriver;
  dalek#exterminate clara;
  peter#talk
