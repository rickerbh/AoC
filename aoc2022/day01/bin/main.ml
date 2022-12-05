open Base
open Stdio

(* Genric reducer *)
let rec reduce fn acc xs =
  match xs with [] -> acc | y :: ys -> fn y (reduce fn acc ys)

(* Split input strings into groups with spaces, and convert to int *)
let grouper (x : string) (acc : int list list) : int list list =
  match x with
  | "" -> List.append acc [ [] ]
  | _ ->
      let y = match List.last acc with Some v -> v | _ -> [] in
      let ys = match List.tl (List.rev acc) with Some v -> v | _ -> [] in
      let new_last = List.append y [ Int.of_string x ] in
      List.append (List.rev ys) [ new_last ]

let sum xs = reduce ( + ) 0 xs
let lines file = In_channel.read_lines file
let input = lines "problem.input"
let groups = reduce grouper [ [] ] input
let summed = List.map ~f:(fun xs -> sum xs) groups
let ordered_calories = List.sort summed ~compare:(Fn.flip compare)

(* Part 1 *)
let max_calories = match List.hd ordered_calories with Some v -> v | _ -> 0

(* Part 2 *)
let a = sum (List.take ordered_calories 3)

let () =
  print_endline
    (String.concat ~sep:"\n"
       [
         String.concat [ "Part 1: "; Caml.string_of_int max_calories ];
         String.concat [ "Part 2: "; Caml.string_of_int a ];
       ])
