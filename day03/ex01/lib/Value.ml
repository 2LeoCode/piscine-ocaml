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
    | [] -> invalid_arg "No next value"
    | head :: next :: _ when head = x -> next
    | _ :: rest -> (next' [@tailcall]) rest
  in
  next' all

let previous x =
  let rec previous' = function
    | [] -> invalid_arg "No previous value"
    | head :: next :: _ when next = x -> head
    | _ :: rest -> (previous' [@tailcall]) rest
  in
  previous' all
