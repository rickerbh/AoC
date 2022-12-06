open Core
open Stdio
module SS = Set.Make (String)

let ( |> ) x f = f x

let explode_string s =
  List.init (String.length s) ~f:(fun i -> String.make 1 (String.get s i))

let lines file = In_channel.read_lines file
let input = List.hd_exn (lines "problem.input")

let rec partition xs n step =
  match xs with
  | [] -> []
  | _ ->
      let this = List.take xs n in
      let next = List.drop xs step in
      if n > List.length next then [ this ]
      else List.append [ this ] (partition (List.drop xs step) n step)

let all_unique xs = List.contains_dup ~compare:String.compare xs

let find_start_marker x ~n =
  List.fold_left x
    ~f:(fun acc v ->
      match acc with
      | Some _, _ -> acc
      | None, x -> if all_unique v then (None, x + 1) else (Some v, x))
    ~init:(None, n)

let get_answer_with_marker_size n =
  partition (explode_string input) n 1 |> find_start_marker ~n |> fun x ->
  match x with Some _, v -> v | None, _ -> -1

let part_1 = get_answer_with_marker_size 4
let part_2 = get_answer_with_marker_size 14

let () =
  print_endline
    (String.concat ~sep:"\n"
       [
         String.concat [ "Part 1: "; Caml.string_of_int part_1 ];
         String.concat [ "Part 2: "; Caml.string_of_int part_2 ];
       ])
