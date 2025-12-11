type rna = Nucleotides.nucleobase list

let rna_to_string =
  let rec rna_to_string' acc = function
    | [] -> acc
    | head :: rest ->
        rna_to_string' (Nucleotides.string_of_nucleobase head ^ acc) rest
  in
  rna_to_string' ""

let[@tail_mod_cons] rec generate_rna : Helix.helix -> rna = function
  | [] -> []
  | (_, _, base) :: rest ->
      (match base with
      | Nucleotides.A -> Nucleotides.U
      | Nucleotides.T -> Nucleotides.A
      | Nucleotides.C -> Nucleotides.G
      | Nucleotides.G -> Nucleotides.C
      | Nucleotides.U -> Nucleotides.U
      | Nucleotides.None -> Nucleotides.None)
      :: (generate_rna [@tailcall]) rest
