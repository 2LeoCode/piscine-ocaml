let () =
  let h = new Ex00.Hydrogen.hydrogen in
  let he = new Ex00.Helium.helium in
  let li = new Ex00.Lithium.lithium in
  let be = new Ex00.Beryllium.beryllium in
  let b = new Ex00.Bore.bore in
  let c = new Ex00.Carbon.carbon in
  let n = new Ex00.Nitrogen.nitrogen in
  let o = new Ex00.Oxygen.oxygen in

  let atoms = [ h; he; li; be; b; c; n; o ] in

  let rec print_atoms = function
    | [] -> ()
    | atom :: rest ->
        print_endline atom#to_string;
        print_atoms rest
  in
  print_atoms atoms;
  print_endline ("not equal ... " ^ if h#equals he then "KO" else "OK");
  print_endline ("not equal ... " ^ if c#equals be then "KO" else "OK");
  print_endline
    ("equal ... " ^ if h#equals (new Ex00.Hydrogen.hydrogen) then "OK" else "KO");
  print_endline
    ("equal ... " ^ if c#equals (new Ex00.Carbon.carbon) then "OK" else "KO")
