open Ex02.Ft_power
open Printf

let print_case = printf "%d\n"

let () =
  ft_power 2 4 |> print_case;
  ft_power 3 0 |> print_case;
  ft_power 0 5 |> print_case
