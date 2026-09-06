class trinitrotoluene =
  object
    inherit
      Molecule.molecule
        "Trinitrotoluene"
        (List.init 3 (fun _ -> new Nitrogen.nitrogen)
        @ List.init 5 (fun _ -> new Hydrogen.hydrogen)
        @ List.init 6 (fun _ -> new Oxygen.oxygen)
        @ List.init 7 (fun _ -> new Carbon.carbon))
  end
