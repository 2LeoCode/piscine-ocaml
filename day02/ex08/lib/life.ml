let generate_protein repr =
  repr |> Helix.string_to_helix |> Rna.generate_rna |> Ribosome.decode_arn
