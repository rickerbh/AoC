open Printf

let problem_1_file = "problem-1.input"

let read_lines name : string list =
  let ic  = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s:: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let file_lines = read_lines problem_1_file

let cartesian x xs =
  List.concat (List.map (fun e -> List.map (fun e' -> (e, e')) xs) x)

let good_sum a = (fst a) + (snd a) = 2020

let answer =
  let line_ints = List.map int_of_string file_lines in
  let answers = List.filter good_sum (cartesian line_ints line_ints) in
  let answer = List.hd answers in
  (fst answer) * (snd answer)
