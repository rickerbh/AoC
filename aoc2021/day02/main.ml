type direction = Forward of int | Up of int | Down of int | Unknown

type sub_pos = { hpos : int; depth : int }

type sub_pos2 = { hpos : int; depth : int; aim : int }

(* OCaml doesn't support \s+ ?!*)
let split_on_space x = Str.split (Str.regexp "[ ]+") x

let direction_generator s i =
  match s with
  | "forward" -> Forward (int_of_string i)
  | "down" -> Down (int_of_string i)
  | "up" -> Up (int_of_string i)
  | _ -> Unknown

let convert_input input =
  match input with
  | [] -> Unknown
  | [ _ ] -> Unknown
  | instruction :: num :: _ -> direction_generator instruction num

let move (acc : sub_pos) d =
  match d with
  | Forward i -> { hpos = acc.hpos + i; depth = acc.depth }
  | Up i -> { hpos = acc.hpos; depth = acc.depth - i }
  | Down i -> { hpos = acc.hpos; depth = acc.depth + i }
  | Unknown -> { hpos = acc.hpos; depth = acc.depth }

let problem_2_1 input =
  let answer =
    List.map split_on_space input
    |> List.map convert_input
    |> List.fold_left move { hpos = 0; depth = 0 }
  in
  answer.hpos * answer.depth

let input = Utils.read_lines "problem_2.input"

let move2 acc d =
  match d with
  | Forward i ->
      { hpos = acc.hpos + i; depth = acc.depth + (acc.aim * i); aim = acc.aim }
  | Up i -> { hpos = acc.hpos; depth = acc.depth; aim = acc.aim - i }
  | Down i -> { hpos = acc.hpos; depth = acc.depth; aim = acc.aim + i }
  | Unknown -> { hpos = acc.hpos; depth = acc.depth; aim = acc.aim }

let problem_2_2 input =
  let answer =
    List.map split_on_space input
    |> List.map convert_input
    |> List.fold_left move2 { hpos = 0; depth = 0; aim = 0 }
  in
  answer.hpos * answer.depth

let test_input =
  [ "forward 5"; "down 5"; "forward 8"; "up 3"; "down 8"; "forward 2" ]

let () =
  let answer = problem_2_1 input in
  print_endline ("Problem 2-1 answer: " ^ string_of_int answer);
  let answer = problem_2_2 input in
  print_endline ("Problem 2-2 answer: " ^ string_of_int answer)
