open Ex03.Deck

let print_case x =
  toStringList x |> List.iter print_endline;
  let card, _ = drawCard x in
  Card.toStringVerbose card |> print_endline;
  Card.getValue card |> Value.toStringVerbose |> print_endline;
  Card.getColor card |> Color.toStringVerbose |> print_endline

let () = newDeck () |> print_case
