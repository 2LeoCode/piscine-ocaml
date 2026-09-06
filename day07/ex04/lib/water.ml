class water =
  object
    inherit
      Molecule.molecule
        "Water"
        (new Oxygen.oxygen :: List.init 2 (fun _ -> new Hydrogen.hydrogen))
  end
