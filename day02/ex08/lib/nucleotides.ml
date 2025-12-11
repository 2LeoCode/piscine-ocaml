type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None
type nucleotide = phosphate * deoxyribose * nucleobase

let string_of_nucleobase = function
  | A -> "A"
  | T -> "T"
  | C -> "C"
  | G -> "G"
  | U -> "U"
  | None -> "."

let nucleobase_of_string = function
  | "A" -> A
  | "T" -> T
  | "C" -> C
  | "G" -> G
  | "U" -> U
  | _ -> None

let generate_nucleotide base : nucleotide =
  let nucleobase_of_char = function
    | 'A' -> A
    | 'T' -> T
    | 'C' -> C
    | 'G' -> G
    | 'U' -> U
    | _ -> None
  in

  ("phosphate", "deoxyribose", nucleobase_of_char base)
