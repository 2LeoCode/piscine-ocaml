type 'a _triplet = 'a * 'a * 'a

let[@tail_mod_cons] rec generate_base_triplets :
    Rna.rna -> Nucleotides.nucleobase _triplet list = function
  | a :: b :: c :: rest ->
      (a, b, c) :: (generate_base_triplets [@tailcall]) rest
  | _ -> []

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

let ( %> ) f g x = x |> f |> g

let[@tail_mod_cons] rec _decode_base_triplets = function
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
      Ala :: (_decode_base_triplets [@tailcall]) rest
  | ( Nucleotides.A, Nucleotides.G, Nucleotides.A
    | Nucleotides.A, Nucleotides.G, Nucleotides.G
    | Nucleotides.C, Nucleotides.G, Nucleotides.A
    | Nucleotides.C, Nucleotides.G, Nucleotides.C
    | Nucleotides.C, Nucleotides.G, Nucleotides.G
    | Nucleotides.C, Nucleotides.G, Nucleotides.U )
    :: rest ->
      Arg :: (_decode_base_triplets [@tailcall]) rest
  | ( Nucleotides.A, Nucleotides.A, Nucleotides.C
    | Nucleotides.A, Nucleotides.A, Nucleotides.U )
    :: rest ->
      Asn :: (_decode_base_triplets [@tailcall]) rest
  | ( Nucleotides.G, Nucleotides.A, Nucleotides.C
    | Nucleotides.G, Nucleotides.A, Nucleotides.U )
    :: rest ->
      Asp :: (_decode_base_triplets [@tailcall]) rest
  | ( Nucleotides.U, Nucleotides.G, Nucleotides.C
    | Nucleotides.U, Nucleotides.G, Nucleotides.U )
    :: rest ->
      Cys :: (_decode_base_triplets [@tailcall]) rest
  | ( Nucleotides.C, Nucleotides.A, Nucleotides.A
    | Nucleotides.C, Nucleotides.A, Nucleotides.G )
    :: rest ->
      Gln :: (_decode_base_triplets [@tailcall]) rest
  | ( Nucleotides.G, Nucleotides.A, Nucleotides.A
    | Nucleotides.G, Nucleotides.A, Nucleotides.G )
    :: rest ->
      Glu :: (_decode_base_triplets [@tailcall]) rest
  | ( Nucleotides.G, Nucleotides.G, Nucleotides.A
    | Nucleotides.G, Nucleotides.G, Nucleotides.C
    | Nucleotides.G, Nucleotides.G, Nucleotides.G
    | Nucleotides.G, Nucleotides.G, Nucleotides.U )
    :: rest ->
      Gly :: (_decode_base_triplets [@tailcall]) rest
  | ( Nucleotides.C, Nucleotides.A, Nucleotides.C
    | Nucleotides.C, Nucleotides.A, Nucleotides.U )
    :: rest ->
      His :: (_decode_base_triplets [@tailcall]) rest
  | ( Nucleotides.A, Nucleotides.U, Nucleotides.A
    | Nucleotides.A, Nucleotides.U, Nucleotides.C
    | Nucleotides.A, Nucleotides.U, Nucleotides.U )
    :: rest ->
      Ile :: (_decode_base_triplets [@tailcall]) rest
  | ( Nucleotides.C, Nucleotides.U, Nucleotides.A
    | Nucleotides.C, Nucleotides.U, Nucleotides.C
    | Nucleotides.C, Nucleotides.U, Nucleotides.G
    | Nucleotides.C, Nucleotides.U, Nucleotides.U
    | Nucleotides.U, Nucleotides.U, Nucleotides.A
    | Nucleotides.U, Nucleotides.U, Nucleotides.G )
    :: rest ->
      Leu :: (_decode_base_triplets [@tailcall]) rest
  | ( Nucleotides.A, Nucleotides.A, Nucleotides.A
    | Nucleotides.A, Nucleotides.A, Nucleotides.G )
    :: rest ->
      Lys :: (_decode_base_triplets [@tailcall]) rest
  | (Nucleotides.A, Nucleotides.U, Nucleotides.G) :: rest ->
      Met :: (_decode_base_triplets [@tailcall]) rest
  | ( Nucleotides.U, Nucleotides.U, Nucleotides.C
    | Nucleotides.U, Nucleotides.U, Nucleotides.U )
    :: rest ->
      Phe :: (_decode_base_triplets [@tailcall]) rest
  | ( Nucleotides.C, Nucleotides.C, Nucleotides.C
    | Nucleotides.C, Nucleotides.C, Nucleotides.A
    | Nucleotides.C, Nucleotides.C, Nucleotides.G
    | Nucleotides.C, Nucleotides.C, Nucleotides.U )
    :: rest ->
      Pro :: (_decode_base_triplets [@tailcall]) rest
  | ( Nucleotides.U, Nucleotides.C, Nucleotides.A
    | Nucleotides.U, Nucleotides.C, Nucleotides.C
    | Nucleotides.U, Nucleotides.C, Nucleotides.G
    | Nucleotides.U, Nucleotides.C, Nucleotides.U
    | Nucleotides.A, Nucleotides.G, Nucleotides.U
    | Nucleotides.A, Nucleotides.G, Nucleotides.C )
    :: rest ->
      Ser :: (_decode_base_triplets [@tailcall]) rest
  | ( Nucleotides.A, Nucleotides.C, Nucleotides.A
    | Nucleotides.A, Nucleotides.C, Nucleotides.C
    | Nucleotides.A, Nucleotides.C, Nucleotides.G
    | Nucleotides.A, Nucleotides.C, Nucleotides.U )
    :: rest ->
      Thr :: (_decode_base_triplets [@tailcall]) rest
  | (Nucleotides.U, Nucleotides.G, Nucleotides.G) :: rest ->
      Trp :: (_decode_base_triplets [@tailcall]) rest
  | ( Nucleotides.U, Nucleotides.A, Nucleotides.C
    | Nucleotides.U, Nucleotides.A, Nucleotides.U )
    :: rest ->
      Tyr :: (_decode_base_triplets [@tailcall]) rest
  | ( Nucleotides.G, Nucleotides.U, Nucleotides.A
    | Nucleotides.G, Nucleotides.U, Nucleotides.C
    | Nucleotides.G, Nucleotides.U, Nucleotides.G
    | Nucleotides.G, Nucleotides.U, Nucleotides.U )
    :: rest ->
      Val :: (_decode_base_triplets [@tailcall]) rest
  | _ :: rest -> (_decode_base_triplets [@tailcall]) rest

let decode_arn : Rna.rna -> protein =
  generate_base_triplets %> _decode_base_triplets
