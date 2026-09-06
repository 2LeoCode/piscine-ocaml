class glucose =
  object
    inherit
      Molecule.molecule
        "Glucose"
        (List.init 12 (fun _ -> new Hydrogen.hydrogen)
        @ List.init 6 (fun _ -> new Carbon.carbon)
        @ List.init 6 (fun _ -> new Oxygen.oxygen))
  end
