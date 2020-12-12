open Containers

let seat_id_of_position position =
  let narrow_down min max =
    max := (!min + !max) / 2
  in
  let narrow_up min max =
    min := (!min + !max) / 2
  in
  let row_min = ref 0 in
  let row_max = ref 128 in (* Not inclusive *)
  for i = 0 to 6 do
    match position.[i] with
    | 'F' -> narrow_down row_min row_max
    | 'B' -> narrow_up row_min row_max
    | _ -> failwith "Unexpected character"
  done;
  let col_min = ref 0 in
  let col_max = ref 9 in (* Not inclusive *)
  for i = 7 to 9 do
    match position.[i] with
    | 'L' -> narrow_down col_min col_max
    | 'R' -> narrow_up col_min col_max
    | _ -> failwith "Unexpected character"
  done;
  !row_min * 8 + !col_min

let task1 input =
  CCIO.read_lines_gen input
  |> Gen.map seat_id_of_position
  |> Gen.fold Int.max 0
  |> Report.int

let total_seats = 1024

let read_free_seats_bitvector input =
  let free_seats = CCBV.create ~size:total_seats true in
  CCIO.read_lines_gen input
  |> Gen.iter (fun position ->
      CCBV.reset free_seats (seat_id_of_position position));
  free_seats

let find_single_free_seat free_seats =
  let exception Found of int in
  let last_seat = total_seats - 1 in
  try
    CCBV.iter_true free_seats (fun index ->
        if index > 0
        && index < last_seat
        && not (CCBV.get free_seats (index - 1))
        && not (CCBV.get free_seats (index + 1))
        then
          raise (Found index));
    None
  with Found x -> Some x

let task2 input =
  read_free_seats_bitvector input
  |> find_single_free_seat
  |> Report.opt_int
