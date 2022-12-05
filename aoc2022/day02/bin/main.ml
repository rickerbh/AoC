open Base
open Stdio

type move = Rock | Paper | Scissors
type outcome = Won | Lost | Draw

let ( |> ) x f = f x

let to_move x =
  match x with
  | "Y" -> Paper
  | "B" -> Paper
  | "X" -> Rock
  | "A" -> Rock
  | _ -> Scissors

let outcome their_move our_move =
  match (their_move, our_move) with
  | Rock, Paper -> Won
  | Paper, Scissors -> Won
  | Scissors, Rock -> Won
  | Rock, Rock -> Draw
  | Paper, Paper -> Draw
  | Scissors, Scissors -> Draw
  | _ -> Lost

let move_points x = match x with Rock -> 1 | Paper -> 2 | Scissors -> 3

let game_points their_move our_move =
  let move_points = move_points our_move in
  let result_points =
    match outcome their_move our_move with Won -> 6 | Draw -> 3 | Lost -> 0
  in
  move_points + result_points

let parse xs =
  List.filter_map xs ~f:(fun x ->
      match String.split x ~on:' ' with
      | [] -> None
      | a :: b -> Some (to_move a, to_move (List.hd_exn b)))

let lines file = In_channel.read_lines file
let input = lines "problem.input"

let calc_games_points input parser_fn =
  List.map (parser_fn input) ~f:(fun (t, u) -> game_points t u)

let sum points =
  match List.reduce points ~f:( + ) with Some v -> v | None -> 0

let part_1 = calc_games_points input parse |> sum

(* Part 2 - derive move from result *)
let to_result x = match x with "Y" -> Draw | "X" -> Lost | _ -> Won

let needed_move their_move outcome =
  match (their_move, outcome) with
  | _, Draw -> their_move
  | Paper, Lost -> Rock
  | Paper, Won -> Scissors
  | Scissors, Lost -> Paper
  | Scissors, Won -> Rock
  | Rock, Lost -> Scissors
  | Rock, Won -> Paper

let parse' xs =
  List.filter_map xs ~f:(fun x ->
      match String.split x ~on:' ' with
      | [] -> None
      | a :: b ->
          Some (to_move a, needed_move (to_move a) (to_result (List.hd_exn b))))

let part_2 = calc_games_points input parse' |> sum

let () =
  print_endline
    (String.concat ~sep:"\n"
       [
         String.concat [ "Part 1: "; Caml.string_of_int part_1 ];
         String.concat [ "Part 2: "; Caml.string_of_int part_2 ];
       ])
