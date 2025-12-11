type 'a _triplet = 'a * 'a * 'a

type aminoacid =
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

let _string_of_aminoacid = function
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
    Rna.rna -> Nucleotides.nucleobase _triplet list = function
  | a :: b :: c :: rest ->
      (a, b, c) :: (generate_base_triplets [@tailcall]) rest
  | _ -> []

let string_of_protein =
  let rec string_of_protein' acc = function
    | [] -> acc
    | head :: rest ->
        (string_of_protein' [@tailcall])
          (acc ^ ", " ^ _string_of_aminoacid head)
          rest
  in
  function
  | [] -> ""
  | head :: rest -> string_of_protein' (_string_of_aminoacid head) rest

let decode_arn arn : protein =
  let[@tail_mod_cons] rec decode_base_triplets = function
    | [] -> []
    | ( Nucleotides.U, Nucleotides.A, Nucleotides.A
      | Nucleotides.U, Nucleotides.A, Nucleotides.G
      | Nucleotides.U, Nucleotides.G, Nucleotides.A )
      :: _ ->
        Stop :: []
    | ( Nucleotides.G, Nucleotides.C, Nucleotides.A
      | Nucleotides.G, Nucleotides.C, Nucleotides.C
      | Nucleotides.G, Nucleotides.C, Nucleotides.G
      | Nucleotides.G, Nucleotides.C, Nucleotides.U )
      :: rest ->
        Ala :: (decode_base_triplets [@tailcall]) rest
    | ( Nucleotides.A, Nucleotides.G, Nucleotides.A
      | Nucleotides.A, Nucleotides.G, Nucleotides.G
      | Nucleotides.C, Nucleotides.G, Nucleotides.A
      | Nucleotides.C, Nucleotides.G, Nucleotides.C
      | Nucleotides.C, Nucleotides.G, Nucleotides.G
      | Nucleotides.C, Nucleotides.G, Nucleotides.U )
      :: rest ->
        Arg :: (decode_base_triplets [@tailcall]) rest
    | ( Nucleotides.A, Nucleotides.A, Nucleotides.C
      | Nucleotides.A, Nucleotides.A, Nucleotides.U )
      :: rest ->
        Asn :: (decode_base_triplets [@tailcall]) rest
    | ( Nucleotides.G, Nucleotides.A, Nucleotides.C
      | Nucleotides.G, Nucleotides.A, Nucleotides.U )
      :: rest ->
        Asp :: (decode_base_triplets [@tailcall]) rest
    | ( Nucleotides.U, Nucleotides.G, Nucleotides.C
      | Nucleotides.U, Nucleotides.G, Nucleotides.U )
      :: rest ->
        Cys :: (decode_base_triplets [@tailcall]) rest
    | ( Nucleotides.C, Nucleotides.A, Nucleotides.A
      | Nucleotides.C, Nucleotides.A, Nucleotides.G )
      :: rest ->
        Gln :: (decode_base_triplets [@tailcall]) rest
    | ( Nucleotides.G, Nucleotides.A, Nucleotides.A
      | Nucleotides.G, Nucleotides.A, Nucleotides.G )
      :: rest ->
        Glu :: (decode_base_triplets [@tailcall]) rest
    | ( Nucleotides.G, Nucleotides.G, Nucleotides.A
      | Nucleotides.G, Nucleotides.G, Nucleotides.C
      | Nucleotides.G, Nucleotides.G, Nucleotides.G
      | Nucleotides.G, Nucleotides.G, Nucleotides.U )
      :: rest ->
        Gly :: (decode_base_triplets [@tailcall]) rest
    | ( Nucleotides.C, Nucleotides.A, Nucleotides.C
      | Nucleotides.C, Nucleotides.A, Nucleotides.U )
      :: rest ->
        His :: (decode_base_triplets [@tailcall]) rest
    | ( Nucleotides.A, Nucleotides.U, Nucleotides.A
      | Nucleotides.A, Nucleotides.U, Nucleotides.C
      | Nucleotides.A, Nucleotides.U, Nucleotides.U )
      :: rest ->
        Ile :: (decode_base_triplets [@tailcall]) rest
    | ( Nucleotides.C, Nucleotides.U, Nucleotides.A
      | Nucleotides.C, Nucleotides.U, Nucleotides.C
      | Nucleotides.C, Nucleotides.U, Nucleotides.G
      | Nucleotides.C, Nucleotides.U, Nucleotides.U
      | Nucleotides.U, Nucleotides.U, Nucleotides.A
      | Nucleotides.U, Nucleotides.U, Nucleotides.G )
      :: rest ->
        Leu :: (decode_base_triplets [@tailcall]) rest
    | ( Nucleotides.A, Nucleotides.A, Nucleotides.A
      | Nucleotides.A, Nucleotides.A, Nucleotides.G )
      :: rest ->
        Lys :: (decode_base_triplets [@tailcall]) rest
    | (Nucleotides.A, Nucleotides.U, Nucleotides.G) :: rest ->
        Met :: (decode_base_triplets [@tailcall]) rest
    | ( Nucleotides.U, Nucleotides.U, Nucleotides.C
      | Nucleotides.U, Nucleotides.U, Nucleotides.U )
      :: rest ->
        Phe :: (decode_base_triplets [@tailcall]) rest
    | ( Nucleotides.C, Nucleotides.C, Nucleotides.C
      | Nucleotides.C, Nucleotides.C, Nucleotides.A
      | Nucleotides.C, Nucleotides.C, Nucleotides.G
      | Nucleotides.C, Nucleotides.C, Nucleotides.U )
      :: rest ->
        Pro :: (decode_base_triplets [@tailcall]) rest
    | ( Nucleotides.U, Nucleotides.C, Nucleotides.A
      | Nucleotides.U, Nucleotides.C, Nucleotides.C
      | Nucleotides.U, Nucleotides.C, Nucleotides.G
      | Nucleotides.U, Nucleotides.C, Nucleotides.U
      | Nucleotides.A, Nucleotides.G, Nucleotides.U
      | Nucleotides.A, Nucleotides.G, Nucleotides.C )
      :: rest ->
        Ser :: (decode_base_triplets [@tailcall]) rest
    | ( Nucleotides.A, Nucleotides.C, Nucleotides.A
      | Nucleotides.A, Nucleotides.C, Nucleotides.C
      | Nucleotides.A, Nucleotides.C, Nucleotides.G
      | Nucleotides.A, Nucleotides.C, Nucleotides.U )
      :: rest ->
        Thr :: (decode_base_triplets [@tailcall]) rest
    | (Nucleotides.U, Nucleotides.G, Nucleotides.G) :: rest ->
        Trp :: (decode_base_triplets [@tailcall]) rest
    | ( Nucleotides.U, Nucleotides.A, Nucleotides.C
      | Nucleotides.U, Nucleotides.A, Nucleotides.U )
      :: rest ->
        Tyr :: (decode_base_triplets [@tailcall]) rest
    | ( Nucleotides.G, Nucleotides.U, Nucleotides.A
      | Nucleotides.G, Nucleotides.U, Nucleotides.C
      | Nucleotides.G, Nucleotides.U, Nucleotides.G
      | Nucleotides.G, Nucleotides.U, Nucleotides.U )
      :: rest ->
        Val :: (decode_base_triplets [@tailcall]) rest
    | _ :: rest -> (decode_base_triplets [@tailcall]) rest
  in

  arn |> generate_base_triplets |> decode_base_triplets
