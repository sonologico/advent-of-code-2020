open Containers

module Intset = Set.Make(Int)

let task1 input =
  let adapters =
    CCIO.read_lines_gen input
    |> Gen.map int_of_string
    |> Gen.to_array
  in
  Array.sort Int.compare adapters;
  let count_1 = ref 0 in
  let count_3 = ref 1 in (* At least one due to the device being last + 3 *)
  let incr diff =
    match diff with
    | 1 -> incr count_1
    | 3 -> incr count_3
    | _ -> ()
  in
  incr adapters.(0); (* wall is 0 *)
  for i = 1 to Array.length adapters - 1 do
    incr (adapters.(i) - adapters.(i - 1))
  done;
  Report.int (!count_1 * !count_3)


let count_valid set =
  let target = Intset.max_elt set + 3 in
  let cache = Hashtbl.create (Intset.cardinal set) in
  let set = Intset.add 0 set in
  let rec f current kont =
    match Hashtbl.get cache current with
    | Some cached -> kont cached
    | None when current = target -> kont 1
    | None when Intset.mem current set ->
      f (current + 1) (fun count_1 ->
          f (current + 2) (fun count_2 ->
              f (current + 3) (fun count_3 ->
                  let count = count_1 + count_2 + count_3 in
                  Hashtbl.add cache current count;
                  kont count)))
    | None -> kont 0
  in
  f 0 Fun.id

let task2 input =
  CCIO.read_lines_gen input
  |> Gen.map int_of_string
  |> Gen.fold (Fun.flip Intset.add) Intset.empty
  |> count_valid
  |> Report.int
