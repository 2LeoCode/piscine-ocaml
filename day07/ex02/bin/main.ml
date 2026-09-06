let () =
  let methane = new Ex02.Methane.methane in
  let ethane = new Ex02.Ethane.ethane in
  let octane = new Ex02.Octane.octane in

  let molecules = [ methane; ethane; octane ] in

  let rec print_molecules = function
    | [] -> ()
    | molecule :: rest ->
        print_endline molecule#to_string;
        print_molecules rest
  in

  print_molecules molecules
