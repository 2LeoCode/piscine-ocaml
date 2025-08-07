open Ex04.Nucleotides
open Printf

let ( %> ) f g x = x |> f |> g

let print_nucleotide (ph, de, base) =
  Printf.printf "%s %s %s\n" ph de
    (match base with
    | Ex04.Nucleotides.A -> "A"
    | Ex04.Nucleotides.T -> "T"
    | Ex04.Nucleotides.C -> "C"
    | Ex04.Nucleotides.G -> "G"
    | Ex04.Nucleotides.None -> "None")

let print_case = generate_nucleotide %> print_nucleotide

let () =
  print_case 'A';
  print_case 'T';
  print_case 'C';
  print_case 'G';
  print_case 'X'
