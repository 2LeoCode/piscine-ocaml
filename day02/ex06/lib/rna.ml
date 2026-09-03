type nucleobase = Nucleotides.nucleobase = A | T | C | G | U | None
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
      | A -> U
      | U -> T
      | T -> A
      | C -> G
      | G -> C
      | None -> None)
      :: (generate_rna [@tailcall]) rest

let () =
  let a = Helix.generate_helix 20 in
  let b = generate_rna a in
  Helix.string_of_helix a |> print_endline;
  string_of_rna b |> print_endline
