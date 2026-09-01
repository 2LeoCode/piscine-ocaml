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

let string_of_nucleobase = function
  | A -> "A"
  | T -> "T"
  | C -> "C"
  | G -> "G"
  | None -> "."
