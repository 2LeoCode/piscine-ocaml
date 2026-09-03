type nucleobase = Nucleotides.nucleobase = A | T | C | G | None
type helix = Nucleotides.nucleotide list

let nucleobase_charset = "ATCG."

let generate_helix : int -> helix =
  Random.self_init ();
  let[@tail_mod_cons] rec generate_helix = function
    | 0 -> []
    | n ->
        Nucleotides.generate_nucleotide nucleobase_charset.[Random.int 5]
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
        match base with A -> T | T -> A | C -> G | G -> C | None -> None )
      :: (complementary_helix [@tailcall]) rest

let () =
  let a = generate_helix 20 in
  let b = complementary_helix a in
  string_of_helix a |> print_endline;
  string_of_helix b |> print_endline
