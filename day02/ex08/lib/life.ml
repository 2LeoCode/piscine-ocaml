let ( %> ) f g x = x |> f |> g

let generate_protein =
  Helix._helix_of_string %> Rna.generate_rna %> Ribosome.decode_arn
