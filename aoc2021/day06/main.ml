let advance school =
  List.fold_left
    (fun acc f -> match f with 0 -> 8 :: 6 :: acc | _ -> pred f :: acc)
    [] school

let rec foldi i f acc = if i <= 0 then acc else foldi (pred i) f (f acc)

let days_count school days = foldi days advance school

let test_input = [ 3; 4; 3; 1; 2 ]

let problem_6_1 input = days_count input 80 |> List.length

(* part 2 blows out ram *)
(* lets look at a frequency based data structure *)

let school input = Utils.frequencies input

let advance2 (school : (int * int) list) =
  List.sort (fun (a, _) (b, _) -> compare b a) school
  |> List.map (fun (k, v) ->
         match k with
         | 0 ->
             let new_sixes = try List.assoc 7 school with Not_found -> 0 in
             [ (8, v); (6, new_sixes + v) ]
         | 7 -> (
             let has_zeros =
               try Option.some (List.assoc 0 school)
               with Not_found -> Option.none
             in
             match has_zeros with None -> [ (6, v) ] | _ -> [])
         | _ -> [ (pred k, v) ])
  |> List.flatten

let problem_6_2 input =
  let school = school input in
  foldi 256 advance2 school |> List.map (fun (_, v) -> v) |> Utils.sum

(* parser *)
let parse_input filename =
  let lines = Utils.read_lines filename in
  List.map
    (fun l -> Str.split (Str.regexp ",") l |> List.map int_of_string)
    lines
  |> List.flatten

let input = parse_input "problem_6.input"

(* Runner *)

let () =
  let answer = problem_6_1 input in
  print_endline ("Problem 6-1 answer: " ^ string_of_int answer);
  let answer = problem_6_2 input in
  print_endline ("Problem 6-2 answer: " ^ string_of_int answer)
