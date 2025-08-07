type rna = Nucleotides.nucleobase list

let _rna_to_string =
  let rec _rna_to_string acc = function
    | [] -> acc
    | head :: rest ->
        _rna_to_string
          (String.make 1 (Nucleotides._nucleobase_to_char head) ^ acc)
          rest
  in
  _rna_to_string ""

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
