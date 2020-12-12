open Containers

type direction = North | South | East | West

type state = {
  north_south : int;
  east_west : int;
  direction : direction;
}

type position = {
  north_south : int;
  east_west : int
}

type instruction_type =
  | N
  | S
  | E
  | W
  | F
  | L
  | R

let instruction_of_string line =
  let instruction_type =
    match line.[0] with
    | 'N' -> N
    | 'S' -> S
    | 'E' -> E
    | 'W' -> W
    | 'F' -> F
    | 'L' -> L
    | 'R' -> R
    | _ -> failwith "unexpected input format"
  in
  (instruction_type, int_of_string (String.sub line 1 (String.length line - 1)))

let manhattan_distance (s : state) =
  Int.abs s.north_south + Int.abs s.east_west

let manhattan_distance_position (p : position) =
  Int.abs p.north_south + Int.abs p.east_west

(* Assumes value is a multiple of 90 *)
let rec turn current value =
  let counterclockwise = function
    | North -> West
    | West -> South
    | South -> East
    | East -> North
  in
  let clockwise = function
    | North -> East
    | West -> North
    | South -> West
    | East -> South
  in
  match value with
  | 0 -> current
  | n when n < 0 -> turn (counterclockwise current) (value + 90)
  | _ -> turn (clockwise current) (value - 90)

let rec task1_step (state : state) = function
  | N, value -> { state with north_south = state.north_south + value }
  | S, value -> { state with north_south = state.north_south - value }
  | E, value -> { state with east_west = state.east_west + value }
  | W, value -> { state with east_west = state.east_west - value }
  | L, value -> { state with direction = turn state.direction (-value) }
  | R, value -> { state with direction = turn state.direction value }
  | F, value ->
    match state.direction with
    | North -> task1_step state (N, value)
    | South -> task1_step state (S, value)
    | East -> task1_step state  (E, value)
    | West -> task1_step state (W, value)

let rec turn_position pos value =
  let counterclockwise { north_south; east_west } =
    {
      east_west = -north_south;
      north_south = east_west;
    }
  in
  let clockwise { north_south; east_west } =
    {
      east_west = north_south;
      north_south = -east_west;
    }
  in
  match value with
  | 0 -> pos
  | n when n < 0 -> turn_position (counterclockwise pos) (value + 90)
  | _ -> turn_position (clockwise pos) (value - 90)

let task2_step (ship, waypoint) = function
  | N, value -> (ship, { waypoint with north_south = waypoint.north_south + value })
  | S, value -> (ship, { waypoint with north_south = waypoint.north_south - value })
  | E, value -> (ship, { waypoint with east_west = waypoint.east_west + value })
  | W, value -> (ship, { waypoint with east_west = waypoint.east_west - value })
  | L, value -> (ship, turn_position waypoint (-value))
  | R, value -> (ship, turn_position waypoint value)
  | F, value ->
    ({
      north_south = ship.north_south + value * waypoint.north_south;
      east_west = ship.east_west + value * waypoint.east_west;
    }, waypoint)

let task1 input =
  let initial_state = { north_south = 0; east_west = 0; direction = East } in
  CCIO.read_lines_gen input
  |> Gen.map instruction_of_string
  |> Gen.fold task1_step initial_state
  |> manhattan_distance
  |> Report.int

let task2 input =
  let ship_state = { north_south = 0; east_west = 0 } in
  let waypoint_state = { north_south = 1; east_west = 10 } in
  CCIO.read_lines_gen input
  |> Gen.map instruction_of_string
  |> Gen.fold task2_step (ship_state, waypoint_state)
  |> Fun.compose fst manhattan_distance_position
  |> Report.int
