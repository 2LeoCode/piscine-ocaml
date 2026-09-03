type nucleobase = Nucleotides.nucleobase = A | T | C | G | U | None
type helix = Nucleotides.nucleotide list

let nucleobase_charset = "ATCGU."

let generate_helix : int -> helix =
  Random.self_init ();
  let[@tail_mod_cons] rec generate_helix = function
    | 0 -> []
    | n ->
        Nucleotides.generate_nucleotide nucleobase_charset.[Random.int 6]
        :: (generate_helix [@tailcall]) (n - 1)
  in
  generate_helix

let string_of_helix : helix -> string =
  let rec string_of_helix acc = function
    | [] -> acc
    | (_, _, base) :: rest ->
        (string_of_helix [@tailcall])
          (Nucleotides.string_of_nucleobase base ^ acc)
          rest
  in
  string_of_helix ""

let[@tail_mod_cons] rec complementary_helix : helix -> helix = function
  | [] -> []
  | (ph, de, base) :: rest ->
      ( ph,
        de,
        match base with
        | A -> T
        | T -> A
        | C -> G
        | G -> C
        | U -> U
        | None -> None )
      :: (complementary_helix [@tailcall]) rest
