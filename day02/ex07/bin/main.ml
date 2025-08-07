open Ex07.Ribosome
open Ex07.Nucleotides

let print_case = decode_arn %> string_of_protein %> print_endline

let () =
  print_case [ U; A; G; A; G; A ];
  print_case [ C; A; A; G; G; G; G; None; A; A; G; A ];
  print_case [ A; U; A; A; A; A; G; U; A; A; C ]
