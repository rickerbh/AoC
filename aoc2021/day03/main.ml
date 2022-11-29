type binary = bool list

let rec int_of_bin = function
  | [] -> 0
  | true :: bs -> 1 + (2 * int_of_bin bs)
  | false :: bs -> 2 * int_of_bin bs

let bool_of_bin_char = function '1' -> true | _ -> false

let int_of_bin_char xs = List.rev xs |> int_of_bin

let bools_of_bin_string s = List.map bool_of_bin_char (Utils.explode s)

let sort_tuple_second_int x y =
  let xv, xc = x in
  let _, yc = y in
  match (xc, yc) with
  | _ when xc > yc -> -1
  | _ when xc < yc -> 1
  | _ -> if xv then -1 else 1

let sort_reverse_tuple_second_int x y =
  let xv, xc = x in
  let _, yc = y in
  match (xc, yc) with
  | _ when xc > yc -> 1
  | _ when xc < yc -> -1
  | _ -> if xv then 1 else -1

let take_first xs = match xs with [] -> None | x :: _ -> Some x

let extract_first t =
  let a, _ = t in
  a

let calculate_problem_3_1_rate counts sort_f =
  counts
  |> List.map (fun x -> List.sort sort_f x)
  |> List.map take_first |> Utils.deoptionalize |> List.map extract_first
  |> int_of_bin_char

let problem_3_1 input =
  let counts =
    List.map bools_of_bin_string input
    |> Utils.transpose_matrix
    |> List.map (fun x -> List.fold_left Utils.count_item [] x)
  in

  let epsilon_rate = calculate_problem_3_1_rate counts sort_tuple_second_int in
  let gamma_rate =
    calculate_problem_3_1_rate counts sort_reverse_tuple_second_int
  in
  epsilon_rate * gamma_rate

let rec calculate_problem_3_2_rate sort_f remaining idx =
  match List.length remaining with
  | 0 -> []
  | 1 -> List.nth remaining 0
  | _ ->
      let counts =
        List.map (fun x -> Utils.drop idx x) remaining
        |> Utils.transpose_matrix
        |> List.map (fun x -> List.fold_left Utils.count_item [] x)
        |> List.map (fun x -> List.sort sort_f x)
        |> take_first
      in
      let first_set =
        match counts with Some xs -> take_first xs | _ -> Option.none
      in
      let first_one =
        (* gross solution - defaults to false, not that it should ever be used though *)
        match first_set with Some xs -> extract_first xs | _ -> false
      in
      let new_remaining =
        List.filter (fun x -> Bool.equal first_one (List.nth x idx)) remaining
      in
      calculate_problem_3_2_rate sort_f new_remaining (idx + 1)

let problem_3_2 input =
  let xs = List.map bools_of_bin_string input in
  let oxy_rate =
    calculate_problem_3_2_rate sort_tuple_second_int xs 0 |> int_of_bin_char
  in
  let cos_rate =
    calculate_problem_3_2_rate sort_reverse_tuple_second_int xs 0
    |> int_of_bin_char
  in
  cos_rate * oxy_rate

let test_input =
  [
    "00100";
    "11110";
    "10110";
    "10111";
    "10101";
    "01111";
    "00111";
    "11100";
    "10000";
    "11001";
    "00010";
    "01010";
  ]

let input = Utils.read_lines "problem_3.input"

let () =
  let answer = problem_3_1 input in
  print_endline ("Problem 3-1 answer: " ^ string_of_int answer);
  let answer = problem_3_2 input in
  print_endline ("Problem 3-2 answer: " ^ string_of_int answer)
