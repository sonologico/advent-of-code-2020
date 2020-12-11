open Containers

module Char_set = Set.Make(Char)
module Char_map = Map.Make(Char)

let count_exists_yes block =
  String.fold
    (fun set c ->
       match c with
       | ' ' -> set
       | _ -> Char_set.add c set)
    Char_set.empty
    block
  |> Char_set.cardinal

let count_for_all_yes block =
  let incr_map map key =
    Char_map.update key (function
        | None -> Some 1
        | Some n -> Some (n + 1))
      map
  in
  let nr_people, yes_count_map = String.fold
    (fun (nr_people, set) c ->
       match c with
       | ' ' -> (nr_people + 1, set)
       | _ -> (nr_people, incr_map set c))
    (1, Char_map.empty)
    block
  in
  Char_map.fold (fun _ yes_count acc ->
      if yes_count = nr_people then
        acc + 1
      else
        acc)
    yes_count_map 0

let report_yes_count counting_method input =
  Utils.read_blocks_gen input
  |> Gen.map counting_method
  |> Gen.fold (+) 0
  |> Report.int

let task1 = report_yes_count count_exists_yes

let task2  = report_yes_count count_for_all_yes

let solutions = [| task1; task2 |]
