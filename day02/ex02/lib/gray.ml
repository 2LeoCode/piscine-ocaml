let gray n =
  assert (n >= 0 && n < Sys.int_size);

  let msb x =
    let rec msb' = function
      | 0 -> 0
      | n -> if x land (1 lsl (n - 1)) = x then n else (msb' [@tailcall]) (n - 1)
    in
    msb' (Sys.int_size - 1)
  in

  let rec print_bits x n =
    if n <> 0 then (
      print_char (if x land (1 lsl (n - 1)) = 0 then '0' else '1');
      (print_bits [@tailcall]) x (n - 1))
  in

  let rec gray' x =
    let v = x lxor (x lsr 1) in
    print_bits v n;
    if msb v <> n then (
      print_char ' ';
      (gray' [@tailcall]) (x + 1))
  in

  gray' 0;
  print_newline ()

let () =
  gray 0;
  gray 1;
  gray 2;
  gray 3;
  gray 4;
  gray 5
