let sequence n =
  let unreachable () = assert false in

  let string_of_int =
    let rec string_of_int' res = function
      | 0 -> res
      | x ->
          let res' =
            (match x mod 10 with
              | 0 -> "0"
              | 1 -> "1"
              | 2 -> "2"
              | 3 -> "3"
              | 4 -> "4"
              | 5 -> "5"
              | 6 -> "6"
              | 7 -> "7"
              | 8 -> "8"
              | 9 -> "9"
              | _ -> unreachable ())
            ^ res
          in
          string_of_int' res' (x / 10)
    in

    string_of_int' ""
  in

  let string_length s =
    let rec string_length' i =
      try
        let _ = s.[i] in
        string_length' (i + 1)
      with Invalid_argument _ -> i
    in
    string_length' 0
  in

  if n <= 0 then ""
  else
    let rec sequence' prev = function
      | 0 -> prev
      | i ->
          let rec next_diff = function
            | i when i = string_length prev -> i
            | i when prev.[i - 1] = prev.[i] -> next_diff (i + 1)
            | i -> i
          in

          let rec next_sequence next = function
            | i when i = string_length prev -> next
            | i ->
                let c =
                  match prev.[i] with
                  | '1' -> "1"
                  | '2' -> "2"
                  | '3' -> "3"
                  | '4' -> "4"
                  | '5' -> "5"
                  | '6' -> "6"
                  | '7' -> "7"
                  | '8' -> "8"
                  | '9' -> "9"
                  | _ -> unreachable ()
                in
                let n = next_diff (i + 1) - i in
                next_sequence (next ^ string_of_int n ^ c) (i + n)
          in

          (next_sequence "" 0 |> sequence') (i - 1)
    in

    sequence' "1" (n - 1)

let () =
  let print_case x = x |> sequence |> print_endline in

  print_case 0;
  print_case 1;
  print_case 2;
  print_case 3;
  print_case 4;
  print_case 5;
  print_case 6;
  print_case 7;
  print_case 8;
  print_case 9;
  print_case 10;
  print_case 11;
  print_case 12;
  print_case 13;
  print_case 14
