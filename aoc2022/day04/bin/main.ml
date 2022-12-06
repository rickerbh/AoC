open Base
open Stdio

let ( |> ) x f = f x
let lines file = In_channel.read_lines file
let input = lines "problem.input"

let parse lines =
  List.map lines ~f:(fun l ->
      let pairs = String.split l ~on:',' in
      List.map pairs ~f:(fun x ->
          let range = String.split x ~on:'-' in
          ( Int.of_string (List.hd_exn range),
            Int.of_string (List.hd_exn (List.tl_exn range)) )))

let is_enclosed a b =
  let a_s, a_e = a in
  let b_s, b_e = b in
  (a_s <= b_s && a_e >= b_e) || (b_s <= a_s && b_e >= a_e)

let find checker_fn xs =
  List.filter xs ~f:(fun l ->
      let a = List.hd_exn l in
      let b = List.hd_exn (List.tl_exn l) in
      checker_fn a b)

let part_1 = parse input |> find is_enclosed |> List.length

(* Part 2 *)
let is_overlapping a b =
  let a_s, a_e = a in
  let b_s, b_e = b in
  (a_s <= b_s && a_e >= b_s) || (b_s <= a_s && b_e >= a_s)

let part_2 = parse input |> find is_overlapping |> List.length

let () =
  print_endline
    (String.concat ~sep:"\n"
       [
         String.concat [ "Part 1: "; Caml.string_of_int part_1 ];
         String.concat [ "Part 2: "; Caml.string_of_int part_2 ];
       ])
