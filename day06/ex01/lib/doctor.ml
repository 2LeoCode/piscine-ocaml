class doctor (name : string) (sidekick : People.people) =
  object (self)
    val name = name
    val mutable age = 0
    val mutable sidekick = sidekick
    val mutable hp = 100
    initializer print_endline (name ^ " has been born with two hearts")

    method to_string =
      "doctor with name = " ^ name ^ ", age = " ^ string_of_int age
      ^ ", sidekick = (" ^ sidekick#to_string ^ "), hp = " ^ string_of_int hp

    method talk = print_endline "Hi! I'm the Doctor!"

    method travel_in_time start arrival =
      age <- age + arrival - start;
      print_endline
        {| ______^_
||""||""||
||__||__||
||""||""||
||__||__||
||""||""||
||__||__||
||""||""||
||__||__||
##########|}

    method use_sonic_screwdriver =
      print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"

    method private regenerate = hp <- 100
  end
