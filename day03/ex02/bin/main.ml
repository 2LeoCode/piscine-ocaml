open Ex02.Card
open Printf

let print_case x = printf "%s : %s\n" (toString x) (toStringVerbose x)
let () = all |> List.iter print_case
