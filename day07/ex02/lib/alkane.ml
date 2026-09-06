class virtual alkane (name : string) n =
  object
    inherit
      Molecule.molecule
        name
        (assert (n > 0);
         List.init n (fun _ -> new Carbon.carbon)
         @ List.init ((2 * n) + 2) (fun _ -> new Hydrogen.hydrogen))
  end
