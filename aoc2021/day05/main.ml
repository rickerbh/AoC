type point = int * int

type line = point * point

let test_input =
  [
    ((0, 9), (5, 9));
    ((8, 0), (0, 8));
    ((9, 4), (3, 4));
    ((2, 2), (2, 1));
    ((7, 0), (7, 4));
    ((6, 4), (2, 0));
    ((0, 9), (2, 9));
    ((3, 4), (1, 4));
    ((0, 0), (8, 8));
    ((5, 5), (8, 2));
  ]

let to_target a b =
  match (a, b) with
  | _, _ when a < b -> a + 1
  | _, _ when a > b -> a - 1
  | _ -> a

let is_horizontal_or_vertical line =
  let (x1, y1), (x2, y2) = line in
  match (x1, y1, x2, y2) with
  | a, _, b, _ when a = b -> true
  | _, a, _, b when a = b -> true
  | _, _, _, _ -> false

let rec generate_line_points line =
  let start_point, end_point = line in
  match start_point = end_point with
  | true -> [ start_point ]
  | _ ->
      let (x1, y1), (x2, y2) = line in
      let next_x = to_target x1 x2 in
      let next_y = to_target y1 y2 in
      start_point :: generate_line_points ((next_x, next_y), end_point)

let expanded_lines input = List.map generate_line_points input

let is_dangerous_vent t = match t with _, f when f > 1 -> true | _, _ -> false

let problem_5_1 input =
  let lines =
    List.filter is_horizontal_or_vertical input
    |> expanded_lines |> List.flatten
  in
  let freqs = Utils.frequencies lines in
  let dangerous_count = List.filter is_dangerous_vent freqs |> List.length in
  dangerous_count

let problem_5_2 input =
  let lines = expanded_lines input |> List.flatten in
  let freqs = Utils.frequencies lines in
  let dangerous_count = List.filter is_dangerous_vent freqs |> List.length in
  dangerous_count

(* Parser *)

let parse_line_to_ints lines =
  let pairs =
    List.map (fun l -> Str.split (Str.regexp " -> ") l) lines
    |> List.map (fun l ->
           List.map
             (fun p ->
               let t = Str.split (Str.regexp ",") p |> List.map int_of_string in
               match t with a :: b :: _ -> Option.some (a, b) | _ -> None)
             l)
    |> List.map Utils.deoptionalize
    |> List.map (fun xs ->
           match xs with a :: b :: _ -> Option.some (a, b) | _ -> None)
    |> Utils.deoptionalize
  in
  pairs

let parse_input filename =
  let lines = Utils.read_lines filename in
  parse_line_to_ints lines

let input = parse_input "problem_5.input"

(* Runner *)

let () =
  let answer = problem_5_1 input in
  print_endline ("Problem 5-1 answer: " ^ string_of_int answer);
  let answer = problem_5_2 input in
  print_endline ("Problem 5-2 answer: " ^ string_of_int answer)
