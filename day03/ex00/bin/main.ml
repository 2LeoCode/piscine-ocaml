open Ex00.Color

let ( %> ) f g x = x |> f |> g
let print_case to_string = to_string %> print_endline

let () =
  all
  |> List.iter (fun x ->
         print_case toString x;
         print_case toStringVerbose x)
