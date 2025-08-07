module Color = struct
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
end

module Value = struct
  type t =
    | T2
    | T3
    | T4
    | T5
    | T6
    | T7
    | T8
    | T9
    | T10
    | Jack
    | Queen
    | King
    | As

  let all = [ T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As ]

  let toInt x =
    let rec toInt' i = function
      | [] -> i
      | head :: _ when head = x -> i
      | _ :: rest -> (toInt' [@tailcall]) (i + 1) rest
    in
    toInt' 1 all

  let toString = function
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
    | As -> "A"

  let toStringVerbose = function
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "Jack"
    | Queen -> "Queen"
    | King -> "King"
    | As -> "As"

  let next x =
    let rec next' = function
      | [] -> invalid_arg "Invalid argument" |> raise
      | head :: next :: _ when head = x -> next
      | _ :: rest -> (next' [@tailcall]) rest
    in
    next' all

  let previous x =
    let rec previous' = function
      | [] -> invalid_arg "Invalid argument" |> raise
      | head :: next :: _ when next = x -> head
      | _ :: rest -> (previous' [@tailcall]) rest
    in
    previous' all
end

module Card = struct
  type t = Value.t * Color.t

  let newCard value color : t = (value, color)

  let allSpades : t list =
    Value.all |> List.map (fun value -> (value, Color.Spade))

  let allHearts : t list =
    Value.all |> List.map (fun value -> (value, Color.Heart))

  let allDiamonds : t list =
    Value.all |> List.map (fun value -> (value, Color.Diamond))

  let allClubs : t list =
    Value.all |> List.map (fun value -> (value, Color.Club))

  let all = allSpades @ allHearts @ allDiamonds @ allClubs
  let getValue : t -> Value.t = function value, _ -> value
  let getColor : t -> Color.t = function _, color -> color

  let toString : t -> string = function
    | value, color -> Value.toString value ^ Color.toString color

  let toStringVerbose : t -> string = function
    | value, color ->
        Printf.sprintf "Card(%s, %s)"
          (Value.toStringVerbose value)
          (Color.toStringVerbose color)

  let compare : t -> t -> int = function
    | lhs, _ -> ( function rhs, _ -> Value.toInt lhs - Value.toInt rhs)

  let max lhs rhs = if compare lhs rhs > 0 then lhs else rhs
  let min lhs rhs = if compare lhs rhs < 0 then lhs else rhs

  let best = function
    | [] -> invalid_arg "List is empty"
    | l -> List.fold_left max (newCard Value.T2 Color.Spade) l

  let isOf : t -> Color.t -> bool = function
    | _, color -> fun expectedColor -> color = expectedColor

  let isSpade x = isOf x Color.Spade
  let isHeart x = isOf x Color.Heart
  let isDiamond x = isOf x Color.Diamond
  let isClub x = isOf x Color.Club
end

type t = Card.t list

let newDeck () =
  Random.self_init ();
  let[@tail_mod_cons] rec newDeck' k a b = function
    | [] ->
        let result = List.rev a @ List.rev b in
        if k = 0 then result else (newDeck' [@tailcall]) (k - 1) [] [] result
    | last :: [] -> (newDeck' [@tailcall]) k (last :: a) b []
    | head :: next :: rest ->
        let a', b' =
          if Random.bool () then (head :: a, next :: b)
          else (next :: a, head :: b)
        in
        (newDeck' [@tailcall]) k a' b' rest
  in
  newDeck' 8 [] [] Card.all

let toStringList = List.map Card.toString
let toStringListVerbose = List.map Card.toStringVerbose

let drawCard = function
  | [] -> Failure "Deck is empty" |> raise
  | card :: rest -> (card, rest)
