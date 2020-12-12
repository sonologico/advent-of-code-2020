open Containers

module String_map = Map.Make(String)
module String_set = Set.Make(String)

let read_rule line =
  let merge_triples list =
    let rec impl acc = function
      | [] -> List.rev acc
      | count :: a :: b :: rest -> impl ((int_of_string count, a ^ " " ^ b) :: acc) rest
      | _ -> failwith "Unexpected rule format"
    in
    impl [] list
  in
  let meaningful_token = function
    | "bag" | "bags" | "bag," | "bags," | "bag." | "bags."
    | ""
    | "contain"
    | "no" | "other" -> false
    | _ -> true
  in
  match
    String.split_on_char ' ' line
    |> List.filter meaningful_token
  with
  | [_; _; "no"; "other"]  -> None
  | pref :: suf :: rest -> Some (pref ^ " " ^ suf, merge_triples rest)
  | _ -> None

let add_rule_reversed rule_map (container, contained_list) =
  List.fold_left (fun rule_map (_count, contained) ->
      String_map.update contained (function
          | None -> Some (String_set.singleton container)
          | Some set -> Some (String_set.add container set))
        rule_map)
    rule_map contained_list

(* Not tail recursive *)
let containers_of bag_type reversed_graph =
  let rec impl bag_type acc =
    if String_set.mem bag_type acc then
      acc
    else
      let acc = String_set.add bag_type acc in
      match String_map.find_opt bag_type reversed_graph with
      | None -> acc
      | Some immediate_set ->
        String_set.fold impl immediate_set acc
  in
  impl bag_type String_set.empty
  |> String_set.remove bag_type

(* Not tail recursive *)
let rec count_contained_bags bag_type forward_graph =
  match String_map.find_opt bag_type forward_graph with
  | None -> 0
  | Some immediate_list ->
    List.fold_left (fun acc (count, bag_type) ->
        acc + (count * (1 + count_contained_bags bag_type forward_graph))
      ) 0 immediate_list

(* Returns map from bag type -> set of containing bag types *)
let read_reverse_graph input =
  CCIO.read_lines_gen input
  |> Gen.filter_map read_rule
  |> Gen.fold add_rule_reversed String_map.empty

(* Returns map from bag type -> list of contained (count, bag types) *)
let read_foward_graph_with_counts input =
  CCIO.read_lines_gen input
  |> Gen.filter_map read_rule
  |> Gen.fold
    (fun acc (container, contained_list) ->
       String_map.add container contained_list acc)
    String_map.empty

let task1 input =
  read_reverse_graph input
  |> containers_of "shiny gold"
  |> String_set.cardinal
  |> Report.int

let task2 input =
  read_foward_graph_with_counts input
  |> count_contained_bags "shiny gold"
  |> Report.int
