open Containers

module Int64map = Map.Make(Int64)

type mask = { zeros : int64; ones : int64; xs : int64 }

type instruction =
  | Mask of mask
  | Mem of { address : int64; value : int64 }

type state = {
  mask : mask;
  memory : int64 Int64map.t;
}

let n_bits = 36

let zero_mask = { zeros = 0L; ones = 0L; xs = 0L }

let apply_mask1 mask value =
  let open Int64 in
  value
  |> logand (lognot mask.zeros)
  |> logor mask.ones

let apply_mask2 mask value =
  let rec f = function
    | [] -> None
    | (value, bit) :: tail when Int64.equal bit (Int64.shift_left 1L 36) -> Some (value, tail)
    | (value, bit) :: tail ->
      let next_bit = Int64.shift_left bit 1 in
      if Int64.equal (Int64.logand bit mask.xs) 0L then
        f ((value, next_bit) :: tail)
      else
        f ((Int64.logor value bit, next_bit)
           :: (Int64.logand value (Int64.lognot bit), next_bit)
           :: tail)
  in
  let base_value = Int64.logor mask.ones value in
  Gen.unfold f [(base_value, 1L)]

let read_mask str =
  let zeros = ref zero_mask.zeros in
  let ones = ref zero_mask.ones in
  let xs = ref zero_mask.xs in
  let last_bit = n_bits - 1 in
  for i = 0 to last_bit do
    let set_bit = Int64.shift_left 1L (last_bit - i) in
    match str.[i] with
    | '0' -> zeros := Int64.logor !zeros set_bit
    | '1' -> ones := Int64.logor !ones set_bit
    | 'X' -> xs := Int64.logor !xs set_bit
    | _ -> failwith ("Unexpected value for mask '" ^ str ^ "'")
  done;
  Mask { zeros = !zeros; ones = !ones; xs = !xs }

let instruction_of_string str =
  match String.split_on_char '=' str |> List.map String.trim with
  | ["mask"; value] -> read_mask value
  | [mem; value] ->
    Mem {
      address = Int64.of_string_exn (String.sub mem 4 (String.length mem - 5));
      value = Int64.of_string_exn value
    }
  | _ -> failwith ("unexpected format: '" ^ str ^ "'")

let read_instructions input =
  CCIO.read_lines_gen input
  |> Gen.map instruction_of_string

let sum_memory state =
  Int64map.fold Int64.(fun _key value acc -> acc + value) state.memory 0L

let store1 state address value =
  {
    state with
    memory = Int64map.add address (apply_mask1 state.mask value) state.memory
  }

let store2 state address value =
  let memory =
    apply_mask2 state.mask address
    |> Gen.fold (fun acc address ->
        Int64map.add address value acc)
      state.memory
  in
  { state with memory }

let execute store_f state instr =
  match instr with
  | Mask x -> { state with mask = x }
  | Mem { address; value } -> store_f state address value

let run store_f input =
  let instructions = read_instructions input in
  let state = { mask = zero_mask; memory = Int64map.empty } in
  Gen.fold (execute store_f) state instructions
  |> sum_memory
  |> Report.int64

let task1 = run store1

let task2 = run store2
