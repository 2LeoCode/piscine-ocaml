type t = Spade | Heart | Diamond | Club

let all = [ Spade; Heart; Diamond; Club ]

let toString = function
  | Spade -> "S"
  | Heart -> "H"
  | Diamond -> "D"
  | Club -> "C"

let toStringVerbose = function
  | Spade -> "Spade"
  | Heart -> "Heart"
  | Diamond -> "Diamond"
  | Club -> "Club"

let () =
  let print_case to_string x = x |> to_string |> print_endline in
  all
  |> List.iter (fun x ->
      print_case toString x;
      print_case toStringVerbose x)
