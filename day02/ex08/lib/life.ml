let invoke_logging msg cb x =
  print_endline msg;
  cb x

let generate_protein repr =
  repr
  |> invoke_logging "creating helix from string" Helix.helix_of_string
  |> invoke_logging "creating rna from helix" Rna.generate_rna
  |> invoke_logging "creating protein from rna" Ribosome.decode_arn

let () =
  let print_case s =
    let output = s |> generate_protein |> Ribosome.string_of_protein in

    print_string "output: ";
    print_endline output
  in

  print_case "..UGT..GU.CUTTCATCTTGGA.UUUGAA.CUTGUG.UUAAU.GTAT.ATUTCGGUGATUC..";
  print_case
    "GTAACUTCAGCATGCTATATUTTCUUCCC.GGAT.TGATUU.UCGUTCTG.UGAUTAA.GTTGGGUAGGUG.TA.CGUGAGCTUGAUCTG.TCAUCTUT.GGT..GUTCAT.ACGCTG.UA.AGAT.A"
