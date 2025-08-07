open Ex05.Hofstadter_mf
open Printf

let ( %> ) f g x = x |> f |> g
let print_case hfs = hfs %> printf "%d\n"

let () =
  print_case hfs_m 0;
  print_case hfs_f 0;
  print_case hfs_m 4;
  print_case hfs_f 4
