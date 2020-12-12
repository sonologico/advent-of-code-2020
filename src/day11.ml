open Containers

type seat =
  | Occupied
  | Free
  | Floor

let seat_equal a b =
  match a, b with
  | Occupied, Occupied
  | Free, Free
  | Floor, Floor -> true
  | _ -> false

let read_line line =
  Array.init (String.length line) (fun i ->
      match line.[i] with
      | '#' -> Occupied
      | 'L' -> Free
      | '.' -> Floor
      | _ -> failwith "unexpected input format")

let read_grid input =
  CCIO.read_lines_gen input
  |> Gen.map read_line
  |> Gen.to_array

let is_occupied grid row col =
  if row >= 0 && row < Array.length grid then
    let col_array = grid.(row) in
    if col >= 0 && col < Array.length col_array then
      match col_array.(col) with
      | Occupied -> true
      | Free | Floor -> false
    else
      false
  else
    false

let occupied_seats_around grid row col =
  let occupied row col =
    Bool.to_int (is_occupied grid row col)
  in
  occupied (row - 1) (col - 1)
  + occupied (row - 1) col
  + occupied (row - 1) (col + 1)
  + occupied row (col - 1)
  + occupied row (col + 1)
  + occupied (row + 1) (col - 1)
  + occupied (row + 1) col
  + occupied (row + 1) (col + 1)

let rec is_direction_occupied grid row col row_step col_step =
  let row = row + row_step in
  let col = col + col_step in
  if row >= 0 && row < Array.length grid then
    let row_array = grid.(row) in
    if col >= 0 && col < Array.length row_array then
      match row_array.(col) with
      | Occupied -> true
      | Free -> false
      | Floor -> is_direction_occupied grid row col row_step col_step
    else
      false
  else
    false

let occupied_seats_seen_from grid row col =
  let occupied row_step col_step =
    Bool.to_int (is_direction_occupied grid row col row_step col_step)
  in
  occupied (-1) (-1)
  + occupied (-1) 0
  + occupied (-1) 1
  + occupied 0 (-1)
  + occupied 0 1
  + occupied 1 (-1)
  + occupied 1 0
  + occupied 1 1

let step rule_f grid =
  let altered = ref false in
  let grid' =
    Array.mapi (fun row row_array ->
        Array.mapi (fun col state ->
            let new_state = rule_f grid row col in
            if not (seat_equal state new_state) then
              altered := true;
            new_state)
          row_array)
      grid
  in
  if !altered then
    Some grid'
  else
    None

let count_occupied grid =
  Array.fold_left (fun acc row_array ->
      Array.fold_left (fun acc cell ->
          match cell with
          | Occupied -> acc + 1
          | _ -> acc)
        acc row_array)
    0 grid

let rec multi_step rule_f grid =
  match step rule_f grid with
  | None -> grid
  | Some grid -> multi_step rule_f grid

let task1_rule grid row col =
  match grid.(row).(col) with
  | Occupied when occupied_seats_around grid row col >= 4 -> Free
  | Free when occupied_seats_around grid row col = 0 -> Occupied
  | x -> x

let task2_rule grid row col =
  match grid.(row).(col) with
  | Occupied when occupied_seats_seen_from grid row col >= 5 -> Free
  | Free when occupied_seats_seen_from grid row col = 0 -> Occupied
  | x -> x

let run rule_f input =
  read_grid input
  |> multi_step rule_f
  |> count_occupied
  |> Report.int

let task1 = run task1_rule

let task2 = run task2_rule
