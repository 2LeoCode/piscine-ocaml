open Ex05.Helix

let () =
  let a = generate_helix 20 in
  let b = complementary_helix a in
  helix_to_string a |> print_endline;
  helix_to_string b |> print_endline
