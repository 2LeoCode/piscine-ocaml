type helix = Nucleotides.nucleotide list

let generate_helix : int -> helix =
  let nucleobase_charset = "ATCG." in
  Random.self_init ();
  let[@tail_mod_cons] rec generate_helix' = function
    | 0 -> []
    | n ->
        Nucleotides.generate_nucleotide nucleobase_charset.[Random.int 5]
        :: (generate_helix' [@tailcall]) (n - 1)
  in
  generate_helix'

let helix_to_string : helix -> string =
  let rec helix_to_string' acc = function
    | [] -> acc
    | (_, _, base) :: rest ->
        (helix_to_string' [@tailcall])
          (Nucleotides.string_of_nucleobase base ^ acc)
          rest
  in

  helix_to_string' ""

let string_to_helix s : helix =
  let len = String.length s in
  let[@tail_mod_cons] rec string_to_helix' = function
    | i when i = len -> []
    | i ->
        Nucleotides.generate_nucleotide s.[i]
        :: (string_to_helix' [@tailcall]) (i + 1)
  in
  string_to_helix' 0

let[@tail_mod_cons] rec complementary_helix : helix -> helix = function
  | [] -> []
  | (ph, de, base) :: rest ->
      ( ph,
        de,
        match base with
        | Nucleotides.A -> Nucleotides.T
        | Nucleotides.T -> Nucleotides.A
        | Nucleotides.C -> Nucleotides.G
        | Nucleotides.G -> Nucleotides.C
        | Nucleotides.U -> Nucleotides.U
        | Nucleotides.None -> Nucleotides.None )
      :: (complementary_helix [@tailcall]) rest
