class oxygen_gas =
  object
    inherit
      Molecule.molecule "Oxygen gaz" (List.init 2 (fun _ -> new Oxygen.oxygen))
  end
