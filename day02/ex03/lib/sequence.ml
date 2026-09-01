let sequence n =
  if n <= 0 then ""
  else
    let rec sequence prev = function
      | 0 -> prev
      | i ->
          let rec next_diff = function
            | i when i = String.length prev -> i
            | i when prev.[i - 1] = prev.[i] -> next_diff (i + 1)
            | i -> i
          in

          let rec next_sequence next = function
            | i when i = String.length prev -> next
            | i ->
                let c = prev.[i] in
                let n = next_diff (i + 1) - i in
                next_sequence (next ^ string_of_int n ^ String.make 1 c) (i + n)
          in

          (next_sequence "" 0 |> sequence) (i - 1)
    in

    sequence "1" (n - 1)

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
  print_case 20
