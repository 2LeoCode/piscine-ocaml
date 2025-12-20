let rec hfs_f = function
  | n when n < 0 -> -1
  | 0 -> 1
  | n -> n - (n - 1 |> hfs_f |> hfs_m)

and hfs_m = function
  | n when n < 0 -> -1
  | 0 -> 0
  | n -> n - (n - 1 |> hfs_m |> hfs_f)

let () =
  let print_case hfs x = x |> hfs |> Printf.printf "%d\n" in

  print_case hfs_f (-1);
  print_case hfs_m (-42);
  print_case hfs_m 0;
  print_case hfs_f 0;
  print_case hfs_m 4;
  print_case hfs_f 4
