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

let cartesian x xs xs' = 
  List.concat (List.map (fun e -> List.concat (List.map (fun e' -> List.map (fun e'' -> (e, e', e'')) xs') xs)) x)

let good_sum x =
  let (a, b, c) = x in 
  a + b + c = 2020

let answer =
  let line_ints = List.map int_of_string file_lines in
  let answers = List.filter good_sum (cartesian line_ints line_ints line_ints) in
  let (a, b, c) = List.hd answers in
  a * b * c 
