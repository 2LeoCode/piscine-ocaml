type helix = Nucleotides.nucleotide list

let _nucleobase_charset = "ATCGU."

let generate_helix : int -> helix =
  Random.self_init ();
  let[@tail_mod_cons] rec generate_helix' = function
    | 0 -> []
    | n ->
        Nucleotides.generate_nucleotide _nucleobase_charset.[Random.int 6]
        :: (generate_helix' [@tailcall]) (n - 1)
  in
  generate_helix'

let helix_to_string : helix -> string =
  let rec helix_to_string' acc = function
    | [] -> acc
    | (_, _, base) :: rest ->
        (helix_to_string' [@tailcall])
          (String.make 1 (Nucleotides._nucleobase_to_char base) ^ acc)
          rest
  in
  helix_to_string' ""

let _helix_of_string s : helix =
  let[@tail_mod_cons] rec _helix_of_string' = function
    | 0 -> []
    | i ->
        Nucleotides.generate_nucleotide s.[i - 1]
        :: (_helix_of_string' [@tailcall]) (i - 1)
  in
  String.length s |> _helix_of_string'

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
