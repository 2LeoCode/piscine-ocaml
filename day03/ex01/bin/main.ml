open Ex01.Value
open Printf

let print_case x =
  printf
    "toInt: %d\n\
     toString: %s\n\
     toStringVerbose: %s\n\
     next.toString: %s\n\
     previous.toString: %s\n"
    (toInt x) (toString x) (toStringVerbose x)
    (try x |> next |> toString with Invalid_argument msg -> msg)
    (try x |> previous |> toString with Invalid_argument msg -> msg)

let () = all |> List.iter print_case
