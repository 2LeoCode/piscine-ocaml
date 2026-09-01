let ft_abs x = if x < 0. then -.x else x
let ft_sq x = x *. x

let ft_sqrt x =
  assert (x >= 0.);
  if x = 0. then 0.
  else
    let delta = 0.00000001 in
    let rec ft_sqrt' ?(y = x /. 2.) () =
      let y' = (y +. (x /. y)) /. 2. in
      if ft_abs (y' -. y) < delta then y' else ft_sqrt' ~y:y' ()
    in

    ft_sqrt' ()

let eu_dist a b =
  assert (Array.length a = Array.length b);

  let rec eu_dist' ?(i = 0) a b =
    if i = Array.length a then 0.
    else ft_sq (a.(i) -. b.(i)) +. eu_dist' ~i:(i + 1) a b
  in

  ft_sqrt (eu_dist' a b)

type radar = float array * string

let one_nn (radars : radar list) (tested_radar : radar) =
  assert (List.length radars <> 0);
  radars
  |> List.fold_left
       (fun acc r ->
         let dist = eu_dist (fst r) (fst tested_radar) in
         if dist < snd acc then (r, dist) else acc)
       (([||], "b"), Float.max_float)
  |> fst |> snd

let () =
  let examples_of_file path : radar list =
    if not (String.ends_with ~suffix:".csv" path) then
      raise (Invalid_argument "invalid csv file path");

    let lines = In_channel.with_open_text path In_channel.input_lines in
    List.map
      (fun line ->
        let cols = String.split_on_char ',' line in
        match List.rev cols with
        | kind :: data ->
            if String.length kind <> 1 then raise Parsing.Parse_error
            else
              ( Array.of_list (List.rev_map (fun x -> float_of_string x) data),
                kind )
        | [] -> raise Parsing.Parse_error)
      lines
  in

  let radars = examples_of_file "attachment/ionosphere.test.csv" in
  Random.self_init ();

  let i = Random.int 350 in
  print_string "Tested_radar: ";
  print_int i;
  print_string ": ";

  let radar = List.nth radars i in

  print_endline (one_nn radars radar)
