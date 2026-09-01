type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = phosphate * deoxyribose * nucleobase

let nucleobase_of_char = function
  | 'A' -> A
  | 'T' -> T
  | 'C' -> C
  | 'G' -> G
  | _ -> None

let generate_nucleotide base : nucleotide =
  ("phosphate", "deoxyribose", nucleobase_of_char base)

let () =
  let print_nucleotide (ph, de, base) =
    Printf.printf "%s %s %s\n" ph de
      (match base with
      | A -> "A"
      | T -> "T"
      | C -> "C"
      | G -> "G"
      | None -> "None")
  in

  let print_case c = c |> generate_nucleotide |> print_nucleotide in

  print_case 'A';
  print_case 'T';
  print_case 'C';
  print_case 'G';
  print_case 'X'
