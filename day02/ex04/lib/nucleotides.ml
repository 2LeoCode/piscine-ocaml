type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = phosphate * deoxyribose * nucleobase

let generate_nucleotide base : nucleotide =
  let nucleobase_of_char = function
    | 'A' -> A
    | 'T' -> T
    | 'C' -> C
    | 'G' -> G
    | _ -> None
  in

  ("phosphate", "deoxyribose", nucleobase_of_char base)
