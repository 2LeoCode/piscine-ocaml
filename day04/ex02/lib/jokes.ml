let main () =
  let jokes =
    [|
      "Why did the chicken cross the road? To get to the other side.";
      "Why did the man put his clock in the oven? He wanted to have a lot of \
       time on his hands.";
      "What do you call a fake noodle? An impasta.";
      "What is brown and sticky? A stick.";
      "Why don’t functional programmers like mutable fields? Because they have \
       trust issues with change.";
    |]
  in
  Random.self_init ();
  print_endline jokes.(Random.int 5)

let () = main ()
