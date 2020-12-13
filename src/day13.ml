open Containers

type bus =
| Id of int
| X

let bus_of_string = function
| "x" -> X
| s -> Id (int_of_string s)

let id = function
| Id x -> Some x
| X -> None

let read_input input =
  let earliest_time =
    CCIO.read_line input
    |> Option.get_exn
    |> int_of_string
  in
  let busses =
    CCIO.read_line input
    |> Option.get_exn
    |> String.split_on_char ','
    |> Array.of_list
    |> Array.map bus_of_string
  in
  (earliest_time, busses)

let lowest_multiple ~higher_than base =
  let rec impl acc =
     if acc < higher_than then
       impl (acc + base)
     else
       acc
  in
  impl base

let task1 input =
  let earliest_time, busses = read_input input in
  let bus_ids = Array.filter_map id busses in
  let (bus_time, bus_id) =
    Array.fold (fun ((min, _min_id) as acc) bus_id ->
        let bus_time = lowest_multiple ~higher_than:earliest_time bus_id in
        if bus_time < min then
          (bus_time, bus_id)
        else
          acc)
      (Int.max_int, -1)
      bus_ids
  in
  Report.int (bus_id * (bus_time - earliest_time))

type id_offset = {
  id : int;
  offset : int;
}

let merge a b =
  let k = ref 0 in
  while (a.id * !k + a.offset) mod b.id <> b.offset do
    incr k;
  done;
  {
    id = a.id * b.id;
    offset = a.id * !k + a.offset;
  }

let find_meeting_point arr =
  let rec safe_mod x modulus =
    if x < 0 then
      safe_mod (x + modulus) modulus
    else
      x mod modulus
  in
  let inverted =
    Array.map (fun x ->
        { id = x.id; offset = safe_mod (x.id - x.offset) x.id })
      arr
  in
  let rec impl acc i =
    if i = Array.length arr then
      acc.offset
    else
      impl (merge acc inverted.(i)) (i + 1)
  in
  impl inverted.(0) 1

let task2 input =
  let _earliest_time, busses = read_input input in
  let id_offsets =
    busses
    |> Array.mapi (fun offset x -> Option.map (fun id -> {id; offset}) (id x))
    |> Array.filter_map Fun.id
  in
  Array.sort (fun a b -> Int.compare b.id a.id) id_offsets;
  find_meeting_point id_offsets
  |> Report.int
