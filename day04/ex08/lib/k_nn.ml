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

module StrMap = Map.Make (String)

let k_nn (radars : radar list) k (tested_radar : radar) =
  assert (List.length radars <> 0);
  let cmp (_, lhs) (_, rhs) =
    if lhs > rhs then -1 else if lhs = rhs then 0 else 1
  in

  let init =
    radars |> List.take k
    |> List.map (fun r -> (r, eu_dist (fst r) (fst tested_radar)))
    |> List.sort cmp
  in
  let rest = radars |> List.drop k in
  let neighbors =
    rest
    |> List.fold_left
         (fun acc r ->
           let dist = eu_dist (fst r) (fst tested_radar) in
           if dist < snd (List.hd acc) then
             (r, dist) :: (acc |> List.drop 1) |> List.sort cmp
           else acc)
         init
    |> List.fold_left
         (fun acc entry ->
           let key = entry |> fst |> snd in
           let value = entry |> snd in
           acc
           |> StrMap.add key
                (if acc |> StrMap.mem key then
                   let found = acc |> StrMap.find key in
                   let cnt = fst found + 1 in
                   let mean = snd found +. (value /. float_of_int cnt) in
                   (cnt, mean)
                 else (1, value)))
         StrMap.empty
  in

  StrMap.fold
    (fun k v acc ->
      let cnt = acc |> snd |> fst in
      let mean = acc |> snd |> snd in
      if fst v > cnt || (fst v = cnt && snd v < mean) then (k, v) else acc)
    neighbors
    ("", (0, Float.max_float))
  |> fst

let () =
  let radars = examples_of_file "attachment/ionosphere.test.csv" in
  Random.self_init ();

  let i = Random.int 350 in
  print_string "Tested_radar: ";
  print_int i;
  print_string ": ";

  let radar = List.nth radars i in

  print_endline (k_nn radars 20 radar)
