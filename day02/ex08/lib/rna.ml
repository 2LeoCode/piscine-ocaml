type rna = Nucleotides.nucleobase list

let string_of_rna =
  let rec string_of_rna acc = function
    | [] -> acc
    | head :: rest ->
        string_of_rna (Nucleotides.string_of_nucleobase head ^ acc) rest
  in
  string_of_rna ""

let[@tail_mod_cons] rec generate_rna : Helix.helix -> rna = function
  | [] -> []
  | (_, _, base) :: rest ->
      (match base with
      | Nucleotides.A -> Nucleotides.U
      | Nucleotides.U -> Nucleotides.T
      | Nucleotides.T -> Nucleotides.A
      | Nucleotides.C -> Nucleotides.G
      | Nucleotides.G -> Nucleotides.C
      | Nucleotides.None -> Nucleotides.None)
      :: (generate_rna [@tailcall]) rest
