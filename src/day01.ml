open Containers

module Int_set = Set.Make(Int)

let sum = 2020

let rec process_numbers1 set gen =
  match gen () with
  | None -> None
  | Some n when Int_set.mem n set -> Some (n * (sum - n))
  | Some n -> process_numbers1 (Int_set.add (sum - n) set) gen

let task1 input =
  CCIO.read_lines_gen input
  |> Gen.map int_of_string
  |> process_numbers1 Int_set.empty
  |> Report.opt_int

module Int_map = Map.Make(Int)

type seen = Once | Twice
type state = {
  seen : seen Int_map.t;
  elligible : int Int_map.t;
}

let update_elligible_from_seen state number =
  Int_map.to_seq state.seen
  |> Seq.fold (fun elliginle (key, _) ->
      let missing = sum - key - number in
      Int_map.add missing (missing * key * number) elliginle
    ) state.elligible

let update_state state number =
  match Int_map.find_opt number state.seen with
  | Some Twice -> state
  | Some Once -> {
      seen = Int_map.add number Twice state.seen;
      elligible = update_elligible_from_seen state number;
    }
  | None -> {
      seen = Int_map.add number Once state.seen;
      elligible = update_elligible_from_seen state number;
    }

let rec process_numbers2 state gen =
  match gen () with
  | None -> None
  | Some number ->
    match Int_map.find_opt number state.elligible with
    | Some result -> Some result
    | None -> process_numbers2 (update_state state number) gen

let task2 input =
  CCIO.read_lines_gen input
  |> Gen.map int_of_string
  |> process_numbers2 { seen = Int_map.empty; elligible = Int_map.empty }
  |> Report.opt_int

let solutions = [| task1; task2 |]
