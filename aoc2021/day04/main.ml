type row = int list

type card = row list

let columns card =
  Utils.transpose_matrix card
  |> List.filter (fun x -> 0 == List.length x |> not)

let row_won numbers row = List.for_all (fun x -> List.mem x numbers) row

let any_true xs =
  List.fold_left (fun acc x -> match acc with true -> true | _ -> x) false xs

let card_won numbers card =
  let rows_won = List.map (row_won numbers) card in
  let columns_won = List.map (row_won numbers) (columns card) in
  any_true (List.append rows_won columns_won)

let winning_cards numbers cards =
  List.filter_map
    (fun c ->
      let has_won = card_won numbers c in
      match has_won with true -> Some c | false -> Option.none)
    cards

let first_winning_card numbers cards =
  let (winning_cards : card list) = winning_cards numbers cards in
  match winning_cards with
  | [] -> Option.none
  | _ -> Some (List.hd winning_cards)

let play_game numbers cards =
  List.fold_left
    (fun (priors, result) x ->
      match result with
      | Some _ -> (priors, result)
      | None ->
          let new_priors = x :: priors in
          let c = first_winning_card new_priors cards in
          (new_priors, c))
    ([], Option.none) numbers

let intersection l1 l2 = List.filter (fun x -> List.mem x l2) l1

let difference l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

let calculate_score numbers card =
  let missing_numbers_sum =
    List.map (fun x -> difference x numbers) card |> List.flatten |> Utils.sum
  in
  let last_number = List.hd numbers in
  missing_numbers_sum * last_number

let problem_4_1 input =
  let numbers, cards = input in
  let winning_numbers, winning_card = play_game numbers cards in
  match winning_card with
  | None -> -1
  | Some c ->
      let score = calculate_score winning_numbers c in
      score

(* Part 2 *)

let play_game_part_2 numbers cards =
  let rec finder priors winners numbers cards =
    match List.length cards with
    | 0 -> (priors, winners)
    | _ -> (
        match numbers with
        | [] -> (priors, []) (* unsolvable *)
        | x :: _ ->
            let new_priors = x :: priors in
            let these_winners = winning_cards new_priors cards in
            let new_winners = winners @ these_winners in
            let remaining_cards = difference cards new_winners in
            finder new_priors new_winners (Utils.drop 1 numbers) remaining_cards
        )
  in
  finder [] [] numbers cards

let problem_4_2 input =
  let numbers, cards = input in
  let winning_numbers, winning_cards = play_game_part_2 numbers cards in
  let winning_card = List.rev winning_cards |> List.hd in
  let score = calculate_score winning_numbers winning_card in
  score

(* Parsing input *)

let parse_card lines =
  List.map
    (fun l -> Str.split (Str.regexp "[ ]+") l |> List.map int_of_string)
    lines

let rec parse_cards lines =
  match lines with
  | [] -> []
  | _ ->
      let card_lines = Utils.take 5 lines in
      let card = parse_card card_lines in
      card :: parse_cards (Utils.drop 6 lines)

let parse_input filename =
  let lines = Utils.read_lines filename in
  let numbers =
    List.hd lines |> Str.split (Str.regexp "[,]") |> List.map int_of_string
  in
  let cards = parse_cards (Utils.drop 2 lines) in
  (numbers, cards)

let input = parse_input "problem_4.input"

let () =
  let answer = problem_4_1 input in
  print_endline ("Problem 4-1 answer: " ^ string_of_int answer);
  let answer = problem_4_2 input in
  print_endline ("Problem 4-2 answer: " ^ string_of_int answer)

(*
   parse_input "./day04/problem_4_test.input"
   problem_4_1
   problem_4_2 (parse_input "./day04/problem_4_test.input")
   
   let a,b = parse_input "./day04/problem_4_test.input" in
   play_game_part_2 a b;

   problem_4_2 (parse_input "./day04/problem_4_test.input")
*)
