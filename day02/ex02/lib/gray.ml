let _msb x =
  let rec msb' = function
    | 0 -> 0
    | n -> if x land (1 lsl (n - 1)) = x then n else (msb' [@tailcall]) (n - 1)
  in
  msb' (Sys.int_size - 1)

let rec _print_bits x n =
  if n <> 0 then (
    print_char (if x land (1 lsl (n - 1)) = 0 then '0' else '1');
    (_print_bits [@tailcall]) x (n - 1))

let gray n =
  assert (n >= 0 && n < Sys.int_size);
  let rec gray' x =
    let v = x lxor (x lsr 1) in
    _print_bits v n;
    if _msb v <> n then (
      print_char ' ';
      gray' (x + 1))
  in
  gray' 0;
  print_newline ()
