let sq x = x *. x

let eu_dist a b =
  assert (Array.length a = Array.length b);

  let rec eu_dist' ?(i = 0) a b =
    if i = Array.length a then 0.
    else sq (a.(i) -. b.(i)) +. eu_dist' ~i:(i + 1) a b
  in

  sqrt (eu_dist' a b)

type radar = float array * string

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
  let radars = examples_of_file "resources/ionosphere.train.csv" in

  let good_1 =
    [|
      1.;
      0.;
      1.;
      0.05812;
      0.94525;
      0.07418;
      0.99952;
      0.13231;
      1.;
      -0.01911;
      0.94846;
      0.07033;
      0.95713;
      0.14644;
      0.94862;
      0.11224;
      0.90896;
      0.20119;
      0.96741;
      0.16265;
      0.99695;
      0.14258;
      0.90784;
      0.16410;
      0.91667;
      0.22431;
      0.88423;
      0.23571;
      0.88568;
      0.22511;
      0.78324;
      0.29576;
      0.83574;
      0.31166;
    |]
  in

  let good_2 =
    [|
      1.;
      0.;
      0.99449;
      0.00526;
      0.84082;
      -0.11313;
      0.88237;
      -0.16431;
      0.99061;
      -0.06257;
      0.96484;
      -0.07496;
      0.85221;
      0.02966;
      0.87161;
      -0.20848;
      0.93881;
      -0.12977;
      0.98298;
      -0.08935;
      0.89876;
      0.00075;
      0.87836;
      -0.05882;
      0.93368;
      -0.19872;
      0.87579;
      -0.17806;
      0.94294;
      -0.16581;
      0.80253;
      -0.25741;
      0.76586;
      -0.27794;
    |]
  in

  let bad_1 =
    [|
      1.;
      0.;
      0.;
      0.;
      0.;
      0.;
      0.;
      0.;
      0.;
      0.;
      0.62195;
      1.;
      0.;
      0.;
      0.;
      0.;
      0.36585;
      -0.71951;
      0.56098;
      -1.;
      0.;
      0.;
      0.;
      0.;
      0.;
      0.;
      1.;
      0.10976;
      0.;
      0.;
      0.;
      0.;
      0.;
      0.;
    |]
  in

  let bad_2 =
    [|
      1.;
      0.;
      0.59840;
      0.40332;
      0.82809;
      0.80521;
      0.76001;
      0.70709;
      0.84010;
      -0.10984;
      0.97311;
      0.07981;
      0.95824;
      -0.85727;
      0.91962;
      0.88444;
      0.95452;
      -0.05206;
      0.88673;
      0.18135;
      0.98484;
      -0.69594;
      0.86670;
      -0.85755;
      0.28604;
      -0.30063;
      1.;
      0.17076;
      0.62958;
      0.42677;
      0.87757;
      0.81007;
      0.81979;
      0.68822;
    |]
  in

  Random.self_init ();
  let random_radars =
    let rec random_radars' acc = function
      | 0 -> acc
      | n ->
          random_radars'
            (Array.init 34 (fun _ -> Random.float 2. -. 1.) :: acc)
            (n - 1)
    in
    random_radars' [] 5
  in

  Printf.printf "Good radars: %s%s\n"
    (one_nn radars (good_1, ""))
    (one_nn radars (good_2, ""));
  Printf.printf "Bad radars: %s%s\n"
    (one_nn radars (bad_1, ""))
    (one_nn radars (bad_2, ""));
  print_string "Random radars: ";
  for i = 0 to 4 do
    print_string (one_nn radars (List.nth random_radars i, ""))
  done;
  print_newline ()
