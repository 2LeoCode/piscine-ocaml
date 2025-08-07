open Ex00.Encode
open Printf

let ( %> ) f g x = x |> f |> g

let print_list print_elem l =
  print_string "[ ";
  List.iter (fun (n, x) -> print_elem n x) l;
  print_endline "]"

let print_int_elem = printf "%d:%d "
let print_string_elem = printf "%d:%s "
let print_case print_elem = encode %> print_list print_elem

let () =
  print_case print_int_elem [ 0; 0; 0; 1; 2; 2; 3; 4; 5; 6; 6; 6; 6; 7; 7; 8 ];
  print_case print_string_elem [ "a"; "a"; "a"; "b"; "c"; "c"; "c" ];
  print_case print_int_elem []
