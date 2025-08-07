type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = phosphate * deoxyribose * nucleobase

let _nucleobase_of_char = function
  | 'A' -> A
  | 'T' -> T
  | 'C' -> C
  | 'G' -> G
  | _ -> None

let _nucleobase_to_char : nucleobase -> char = function
  | A -> 'A'
  | T -> 'T'
  | C -> 'C'
  | G -> 'G'
  | None -> '.'

let generate_nucleotide base : nucleotide =
  ("phosphate", "deoxyribose", _nucleobase_of_char base)
