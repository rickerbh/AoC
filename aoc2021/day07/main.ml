let test_input = [ 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 ]

let middle input =
  let list = List.sort compare input in
  let half_length = List.length list / 2 in
  List.nth (Utils.drop half_length list) 0

let fuel_usage position crabs =
  List.map (fun x -> if x > position then x - position else position - x) crabs
  |> Utils.sum

let problem_7_1 input = fuel_usage (middle input) input

let list_min xs = List.sort compare xs |> List.hd

let list_max xs = List.sort compare xs |> List.rev |> List.hd

let range s e = List.init (e - s) (fun x -> pred s + x + 1)

let rec crabby_fuel i = match i with 0 -> 0 | _ -> i + crabby_fuel (pred i)

let fuel_usage2 position crabs =
  List.map
    (fun x ->
      if x > position then crabby_fuel (x - position)
      else crabby_fuel (position - x))
    crabs
  |> Utils.sum

let problem_7_2 input =
  let xs = range (list_min input) (succ (list_max input)) in
  List.fold_left
    (fun acc v ->
      let this_fuel = fuel_usage2 v input in
      if acc < this_fuel then acc else this_fuel)
    Int.max_int xs

(* parser *)
let parse_input filename =
  let lines = Utils.read_lines filename in
  List.map
    (fun l -> Str.split (Str.regexp ",") l |> List.map int_of_string)
    lines
  |> List.flatten

let input = parse_input "problem_7.input"

(* Runner *)

let () =
  let answer = problem_7_1 input in
  print_endline ("Problem 7-1 answer: " ^ string_of_int answer);
  let answer = problem_7_2 input in
  print_endline ("Problem 7-2 answer: " ^ string_of_int answer)
