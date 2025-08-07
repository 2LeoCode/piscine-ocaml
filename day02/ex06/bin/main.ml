open Ex06.Helix
open Ex06.Rna

let () =
  let a = generate_helix 20 in
  let b = generate_rna a in
  helix_to_string a |> print_endline;
  _rna_to_string b |> print_endline
