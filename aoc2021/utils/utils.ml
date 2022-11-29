let rec drop n list =
  match (n, list) with
  | 0, list -> list
  | _, [] -> []
  | _, _ :: xs -> drop (n - 1) xs

let rec take n list =
  match (n, list) with
  | 0, _ -> []
  | _, [] -> []
  | _, x :: xs -> x :: take (n - 1) xs

let exhaust ic =
  let all_input = ref [] in
  (try
     while true do
       all_input := input_line ic :: !all_input
     done
   with End_of_file -> ());
  close_in ic;
  List.rev !all_input

let read_lines path = open_in path |> exhaust

let deoptionalize l =
  let rec deopt acc = function
    | [] -> List.rev acc
    | None :: tl -> deopt acc tl
    | Some x :: tl -> deopt (x :: acc) tl
  in
  deopt [] l

let rec first_elems ys =
  match ys with (x :: _) :: xss -> x :: first_elems xss | _ -> []

let rec rest_elems ys =
  match ys with (_ :: xs) :: xss -> xs :: rest_elems xss | _ -> []

let rec transpose_matrix xs =
  match xs with
  | [] -> []
  | _ ->
      let rest = rest_elems xs in
      first_elems xs :: transpose_matrix rest

let sum xs = List.fold_left ( + ) 0 xs

let count_item acc v =
  let count = List.assoc_opt v acc in
  match count with
  | Some x -> (v, x + 1) :: List.remove_assoc v acc
  | None -> (v, 1) :: acc

let frequencies xs = List.fold_left count_item [] xs

let explode s = List.init (String.length s) (String.get s)

let intersection l1 l2 = List.filter (fun x -> List.mem x l2) l1

let difference l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1
