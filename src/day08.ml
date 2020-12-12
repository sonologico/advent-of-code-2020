open Containers

module Intset = Set.Make(Int)

type instruction =
  | Nop of int
  | Acc of int
  | Jmp of int

type interpreter_state = {
  previously_executed : Intset.t;
  code : instruction array;
  accumulator : int;
  position : int;
}

type safe_step_output =
  | Halted
  | Loop_detected
  | Step of interpreter_state

let step state =
  let previously_executed = Intset.add state.position state.previously_executed in
  match state.code.(state.position) with
  | Nop _ ->  {
      state with
      previously_executed;
      position = state.position + 1;
    }
  | Acc x ->  {
      state with
      previously_executed;
      accumulator = state.accumulator + x;
      position = state.position + 1
    }
  | Jmp x -> {
      state with
      previously_executed;
      position = state.position + x;
    }

let safe_step state =
  if state.position < Array.length state.code then
    if Intset.mem state.position state.previously_executed then
      Loop_detected
    else
      Step (step state)
  else
    Halted

let create_interpreter code =
  {
    code;
    previously_executed = Intset.empty;
    accumulator = 0;
    position = 0;
  }

type multi_step_output =
  | Halted of interpreter_state
  | Loop_detected of interpreter_state

let rec safe_multi_step state =
  match safe_step state with
  | Step state -> safe_multi_step state
  | Halted -> Halted state
  | Loop_detected -> Loop_detected state

let read_instruction x =
  match String.split_on_char ' ' x with
  | ["nop"; n] -> Nop (int_of_string n)
  | ["acc"; n] -> Acc (int_of_string n)
  | ["jmp"; n] -> Jmp (int_of_string n)
  | _ -> failwith "Unexpected instruction format"

let read_code input =
  CCIO.read_lines_gen input
  |> Gen.map read_instruction
  |> Gen.to_array

let task1 input =
  let interpreter = create_interpreter (read_code input) in
  match safe_multi_step interpreter with
  | Loop_detected interpreter -> Report.int interpreter.accumulator
  | _ -> failwith "Unexpected state"

let possible_programs code =
  let rec f index =
    if index < Array.length code then
      match code.(index) with
      | Acc _ -> f (index + 1)
      | Nop x ->
        let code = Array.copy code in
        code.(index) <- Jmp x;
        Some (code, index + 1)
      | Jmp x ->
        let code = Array.copy code in
        code.(index) <- Nop x;
        Some (code, index + 1)
    else
      None
  in
  Gen.unfold f 0

let task2 input =
  read_code input
  |> possible_programs
  |> Gen.map create_interpreter
  |> Gen.map safe_multi_step
  |> Gen.filter_map (function Halted x -> Some x.accumulator | _ -> None)
  |> Gen.next
  |> Report.opt_int

let solutions = [| task1; task2 |]
