let input = Utils.read_lines "problem_1.input" |> List.map int_of_string

let increment_counter (prior, inc_count) x =
  match x with _ when x > prior -> (x, inc_count + 1) | _ -> (x, inc_count)

let problem_1_1 input =
  let _, result = List.fold_left increment_counter (List.hd input, 0) input in
  result

let rec partitioner xs =
  match xs with
  | [] -> []
  | _ when List.length xs < 3 -> []
  | _ -> Utils.take 3 xs :: partitioner (Utils.drop 1 xs)

let test_input = [ 199; 200; 208; 210; 200; 207; 240; 269; 260; 263 ]

let () =
  let answer = problem_1_1 input in
  print_endline ("Problem 1-1 answer: " ^ string_of_int answer);
  let answer2 = partitioner input |> List.map Utils.sum |> problem_1_1 in
  print_endline ("Problem 1-2 answer: " ^ string_of_int answer2)
