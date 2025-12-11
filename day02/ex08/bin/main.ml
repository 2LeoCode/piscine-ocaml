open Ex08.Life
open Ex08.Ribosome

let ( %> ) f g x = x |> f |> g
let print_case = generate_protein %> string_of_protein %> print_endline

let () =
  print_case "..UGT..GU.CUTTCATCTTGGA.UUUGAA.CUTGUG.UUAAU.GTAT.ATUTCGGUGATUC..";
  print_case
    "GTAACUTCAGCATGCTATATUTTCUUCCC.GGAT.TGATUU.UCGUTCTG.UGAUTAA.GTTGGGUAGGUG.TA.CGUGAGCTUGAUCTG.TCAUCTUT.GGT..GUTCAT.ACGCTG.UA.AGAT.A"
