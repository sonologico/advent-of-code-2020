open Containers

module Intmap = Map.Make(Int)

type state = {
  number_positions : int Intmap.t;
  last : int;
  position : int;
}

let initial_state n = {
  number_positions = Intmap.empty;
  last = n;
  position = 1;
}

let add_number state n = {
  number_positions =
    Intmap.add state.last state.position state.number_positions;
  last = n;
  position = state.position + 1;
}

let step state =
  match Intmap.find_opt state.last state.number_positions with
  | None -> add_number state 0
  | Some position -> add_number state (state.position - position)

let report_nth_number nth input =
  let numbers =
    CCIO.read_all input
    |> String.trim
    |> String.split_on_char ','
    |> List.map int_of_string
  in
  let end_state =
    List.fold_left add_number (initial_state (List.hd numbers)) (List.tl numbers)
    |> Fun.iterate (nth - List.length numbers) step
  in
  Report.int end_state.last

let task1 = report_nth_number 2020

let task2 = report_nth_number 30000000
