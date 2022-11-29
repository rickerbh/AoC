let number_finder s =
  if String.length s = 2 then Option.some 1
  else if String.length s = 4 then Option.some 4
  else if String.length s = 3 then Option.some 7
  else if String.length s = 7 then Option.some 8
  else Option.none

(* 0 1 2 3 4 5   7 8 *)
let problem_8_1 input =
  List.map number_finder input |> deoptionalize |> List.length

let top_finder xs =
  let ys =
    List.sort (fun x y -> compare (List.length x) (List.length y)) xs |> take 2
  in
  difference (List.nth ys 1) (List.nth ys 0) |> List.hd

let one xs = List.filter (fun x -> List.length x = 2) xs |> List.hd

let three xs =
  let one = one xs in
  let maybe_threes = List.filter (fun x -> List.length x = 5) xs in
  List.filter (fun x -> List.length (intersection one x) = 2) maybe_threes
  |> List.hd

let four xs = List.filter (fun x -> List.length x = 4) xs |> List.hd

let seven xs = List.filter (fun x -> List.length x = 3) xs |> List.hd

let eight xs = List.filter (fun x -> List.length x = 7) xs |> List.hd

let zero xs =
  let one = one xs in
  let four_diff = difference (four xs) one in
  let maybe_zeros = List.filter (fun x -> List.length x = 6) xs in
  List.filter (fun x -> List.length (difference four_diff x) = 1) maybe_zeros
  |> List.hd

let top_left_finder xs =
  let one = one xs in
  let four_diff = difference (four xs) one in
  let zero = zero xs in
  intersection four_diff zero |> List.hd

let two xs =
  let top_left = top_left_finder xs in
  let three = three xs in
  let maybe_fives = List.filter (fun x -> List.length x = 5) xs in
  let two_or_five =
    List.filter (fun x -> List.length (difference x three) != 0) maybe_fives
  in
  List.filter
    (fun x -> List.length (intersection [ top_left ] x) = 0)
    two_or_five
  |> List.hd

let five xs =
  let top_left = top_left_finder xs in
  let three = three xs in
  let maybe_fives = List.filter (fun x -> List.length x = 5) xs in
  let two_or_five =
    List.filter (fun x -> List.length (difference x three) != 0) maybe_fives
  in
  List.filter
    (fun x -> List.length (intersection [ top_left ] x) = 1)
    two_or_five
  |> List.hd

let middle_finder xs =
  let one = one xs in
  let four_diff = difference (four xs) one in
  let maybe_zeros = List.filter (fun x -> List.length x = 6) xs in
  let zero =
    List.filter (fun x -> List.length (difference four_diff x) = 1) maybe_zeros
    |> List.hd
  in
  difference four_diff zero |> List.hd

let bottom_finder xs =
  let four = four xs in
  let seven = seven xs in
  let three = three xs in
  difference (difference three four) seven |> List.hd

(* parser *)

let parse_part_1 input =
  List.map
    (fun xs -> match xs with _ :: a -> Option.some a | _ -> Option.none)
    input
  |> deoptionalize |> List.flatten
  |> List.map (fun l -> Str.split (Str.regexp " ") l)
  |> List.flatten

let parse_part_2 input =
  List.map
    (fun xs ->
      match xs with
      | a :: b :: _ ->
          Option.some
            ( List.map explode (Str.split (Str.regexp " ") a),
              Str.split (Str.regexp " ") b )
      | _ -> Option.none)
    input
  |> deoptionalize

let parse_input filename =
  let lines = read_lines filename in
  List.map (fun l -> Str.split (Str.regexp " | ") l) lines

let input = parse_input "day08/problem_8.input"

let input =
  [
    [
      "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab";
      "cdfeb fcadb cdfeb cdbaf";
    ];
  ]

let part_1_input = parse_part_1 input

let part_2_input = parse_part_2 input

let six xs =
  let maybe_sixes = List.filter (fun x -> List.length x = 6) xs in
  let zero = zero xs in
  let six_or_nine =
    List.filter (fun x -> List.length (difference x zero) != 0) maybe_sixes
  in
  six_or_nine

(*
let (a,b) = List.nth part_2_input 0 in
let top_segment = top_finder a in
let top_left_segment = top_left_finder a in
let middle_segment = middle_finder a in
let bottom_segment = bottom_finder a in
let x = six a in
x

('*', '*', bottom_segment, top_segment, top_left_segment, middle_segment, '*')

*)

(* Runner *)

(*
let () =
  let answer = problem_8_1 part_1_input in
  print_endline ("Problem 8-1 answer: " ^ string_of_int answer);
  let answer = problem_8_2 input in
  print_endline ("Problem 8-2 answer: " ^ string_of_int answer)
*)