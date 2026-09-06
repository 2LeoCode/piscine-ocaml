class carbon_dioxyde =
  object
    inherit
      Molecule.molecule
        "Carbon dioxyde"
        (new Carbon.carbon :: List.init 2 (fun _ -> new Oxygen.oxygen))
  end
