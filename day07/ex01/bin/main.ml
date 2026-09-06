let () =
  let water = new Ex01.Water.water in
  let carbon_dioxyde = new Ex01.Carbon_dioxyde.carbon_dioxyde in
  let trinitrotoluene = new Ex01.Trinitrotoluene.trinitrotoluene in
  let oxygen_gas = new Ex01.Oxygen_gas.oxygen_gas in
  let glucose = new Ex01.Glucose.glucose in

  let molecules =
    [ water; carbon_dioxyde; trinitrotoluene; oxygen_gas; glucose ]
  in

  let rec print_molecules = function
    | [] -> ()
    | molecule :: rest ->
        print_endline molecule#to_string;
        print_molecules rest
  in

  print_molecules molecules;
  print_endline ("not equal ... " ^ if water#equals glucose then "KO" else "OK");
  print_endline
    ("not equal ... " ^ if trinitrotoluene#equals oxygen_gas then "KO" else "OK");
  print_endline
    ("equal ... " ^ if water#equals (new Ex01.Water.water) then "OK" else "KO");
  print_endline
    ("equal ... "
    ^
    if trinitrotoluene#equals (new Ex01.Trinitrotoluene.trinitrotoluene) then
      "OK"
    else "KO")
