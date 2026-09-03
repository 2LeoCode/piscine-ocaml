type nucleobase = Nucleotides.nucleobase = A | T | C | G | U | None
type 'a triplet = 'a * 'a * 'a

type aminoacid =
  | Unknown
  | Stop
  | Ala
  | Arg
  | Asn
  | Asp
  | Cys
  | Gln
  | Glu
  | Gly
  | His
  | Ile
  | Leu
  | Lys
  | Met
  | Phe
  | Pro
  | Ser
  | Thr
  | Trp
  | Tyr
  | Val

let string_of_aminoacid = function
  | Unknown -> "?"
  | Stop -> "End of translation"
  | Ala -> "Alanine"
  | Arg -> "Arginine"
  | Asn -> "Asparagine"
  | Asp -> "Aspartique"
  | Cys -> "Cysteine"
  | Gln -> "Glutamine"
  | Glu -> "Glutamique"
  | Gly -> "Glycine"
  | His -> "Histidine"
  | Ile -> "Isoleucine"
  | Leu -> "Leucine"
  | Lys -> "Lysine"
  | Met -> "Methionine"
  | Phe -> "Phenylalanine"
  | Pro -> "Proline"
  | Ser -> "Serine"
  | Thr -> "Threonine"
  | Trp -> "Tryptophane"
  | Tyr -> "Tyrosine"
  | Val -> "Valine"

type protein = aminoacid list

let[@tail_mod_cons] rec generate_base_triplets :
    Rna.rna -> nucleobase triplet list = function
  | a :: b :: c :: rest ->
      (a, b, c) :: (generate_base_triplets [@tailcall]) rest
  | _ -> []

let string_of_protein =
  let rec string_of_protein acc = function
    | [] -> acc
    | head :: rest ->
        (string_of_protein [@tailcall])
          (acc ^ ", " ^ string_of_aminoacid head)
          rest
  in
  function
  | [] -> ""
  | head :: rest -> string_of_protein (string_of_aminoacid head) rest

let decode_arn arn : protein =
  let[@tail_mod_cons] rec decode_base_triplets = function
    | [] -> []
    | (U, A, A | U, A, G | U, G, A) :: _ -> Stop :: []
    | (G, C, A | G, C, C | G, C, G | G, C, U) :: rest ->
        Ala :: (decode_base_triplets [@tailcall]) rest
    | (A, G, A | A, G, G | C, G, A | C, G, C | C, G, G | C, G, U) :: rest ->
        Arg :: (decode_base_triplets [@tailcall]) rest
    | (A, A, C | A, A, U) :: rest ->
        Asn :: (decode_base_triplets [@tailcall]) rest
    | (G, A, C | G, A, U) :: rest ->
        Asp :: (decode_base_triplets [@tailcall]) rest
    | (U, G, C | U, G, U) :: rest ->
        Cys :: (decode_base_triplets [@tailcall]) rest
    | (C, A, A | C, A, G) :: rest ->
        Gln :: (decode_base_triplets [@tailcall]) rest
    | (G, A, A | G, A, G) :: rest ->
        Glu :: (decode_base_triplets [@tailcall]) rest
    | (G, G, A | G, G, C | G, G, G | G, G, U) :: rest ->
        Gly :: (decode_base_triplets [@tailcall]) rest
    | (C, A, C | C, A, U) :: rest ->
        His :: (decode_base_triplets [@tailcall]) rest
    | (A, U, A | A, U, C | A, U, U) :: rest ->
        Ile :: (decode_base_triplets [@tailcall]) rest
    | (C, U, A | C, U, C | C, U, G | C, U, U | U, U, A | U, U, G) :: rest ->
        Leu :: (decode_base_triplets [@tailcall]) rest
    | (A, A, A | A, A, G) :: rest ->
        Lys :: (decode_base_triplets [@tailcall]) rest
    | (A, U, G) :: rest -> Met :: (decode_base_triplets [@tailcall]) rest
    | (U, U, C | U, U, U) :: rest ->
        Phe :: (decode_base_triplets [@tailcall]) rest
    | (C, C, C | C, C, A | C, C, G | C, C, U) :: rest ->
        Pro :: (decode_base_triplets [@tailcall]) rest
    | (U, C, A | U, C, C | U, C, G | U, C, U | A, G, U | A, G, C) :: rest ->
        Ser :: (decode_base_triplets [@tailcall]) rest
    | (A, C, A | A, C, C | A, C, G | A, C, U) :: rest ->
        Thr :: (decode_base_triplets [@tailcall]) rest
    | (U, G, G) :: rest -> Trp :: (decode_base_triplets [@tailcall]) rest
    | (U, A, C | U, A, U) :: rest ->
        Tyr :: (decode_base_triplets [@tailcall]) rest
    | (G, U, A | G, U, C | G, U, G | G, U, U) :: rest ->
        Val :: (decode_base_triplets [@tailcall]) rest
    | (_, _, _) :: rest -> Unknown :: (decode_base_triplets [@tailcall]) rest
  in

  arn |> generate_base_triplets |> decode_base_triplets
