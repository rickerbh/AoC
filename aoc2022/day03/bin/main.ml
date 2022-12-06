open Core
open Stdio
module SS = Set.Make (String)

let ( |> ) x f = f x

let explode_string s =
  List.init (String.length s) ~f:(fun i -> String.make 1 (String.get s i))

let sum points =
  match List.reduce points ~f:( + ) with Some v -> v | None -> 0

let split_lines xs =
  List.map xs ~f:(fun x ->
      let half_length = String.length x / 2 in
      ( String.sub x ~pos:0 ~len:half_length,
        String.sub x ~pos:half_length ~len:half_length ))

let lines file = In_channel.read_lines file
let input = lines "problem.input"

let to_tuple_sets a =
  List.map a ~f:(fun (a, b) ->
      let a_set = SS.of_list (explode_string a) in
      let b_set = SS.of_list (explode_string b) in
      (a_set, b_set))

let diffs xs = List.map xs ~f:(fun (a, b) -> SS.to_list (SS.inter a b))

let priority s =
  let c = String.get s 0 in
  match Char.is_uppercase c with
  | true -> Char.to_int c - 38
  | false -> Char.to_int c - 96

let score xs = List.map xs ~f:(fun x -> List.hd_exn x |> priority)
let part_1 = split_lines input |> to_tuple_sets |> diffs |> score |> sum

(* Part 2 *)

let rec partition_all xs n =
  match xs with
  | [] -> []
  | _ ->
      let this, next = List.split_n xs n in
      List.append [ this ] (partition_all next n)

(* Transform the arrays of 3 strings into tuples of 3 string sets.
   There is probably a nicer way to do this. *)
let to_tuple_sets' xs =
  List.map xs ~f:(fun x ->
      let a_set = SS.of_list (explode_string (List.hd_exn x)) in
      let b_set = SS.of_list (explode_string (List.hd_exn (List.tl_exn x))) in
      let c_set =
        SS.of_list (explode_string (List.hd_exn (List.tl_exn (List.tl_exn x))))
      in
      (a_set, b_set, c_set))

let three_set_intersection (a, b, c) = SS.to_list (SS.inter a (SS.inter b c))

let part_2 =
  partition_all input 3 |> to_tuple_sets'
  |> List.map ~f:three_set_intersection
  |> score |> sum

let () =
  print_endline
    (String.concat ~sep:"\n"
       [
         String.concat [ "Part 1: "; Caml.string_of_int part_1 ];
         String.concat [ "Part 2: "; Caml.string_of_int part_2 ];
       ])
