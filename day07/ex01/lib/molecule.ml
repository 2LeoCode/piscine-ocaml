class virtual molecule (name : string) (atoms : Atom.atom list) =
  let atom_db =
    [
      new Carbon.carbon;
      new Hydrogen.hydrogen;
      new Bore.bore;
      new Beryllium.beryllium;
      new Helium.helium;
      new Lithium.lithium;
      new Nitrogen.nitrogen;
      new Oxygen.oxygen;
    ]
  in

  object
    val formula =
      List.fold_left
        (fun acc atom ->
          let count =
            List.fold_left
              (fun acc atom2 -> acc + if atom#equals atom2 then 1 else 0)
              0 atoms
          in
          if count = 0 then acc
          else
            Printf.sprintf "%s%s" acc atom#symbol
            ^ if count == 1 then "" else string_of_int count)
        "" atom_db

    method name = name
    method formula = formula

    method to_string =
      Printf.sprintf "molecule with name = %s, formula = %s" name formula

    method equals (other : molecule) = formula = other#formula
  end
