open Containers

let size = 25

type state = {
  buffer : int array;
  mutable position : int;
  sums : (int, (int, int) Hashtbl.t) Hashtbl.t;
}

let numbers_gen input =
  CCIO.read_lines_gen input
  |> Gen.map int_of_string

let incr_sum_ref state a b =
  let sum = a + b in
  let inner_tbl = Hashtbl.get_or_add
      state.sums
      ~k:sum
      ~f:(fun _ -> Hashtbl.create 32)
  in
  Hashtbl.incr inner_tbl a;
  Hashtbl.incr inner_tbl b

let decr_sum_ref state a b =
  let decr tbl key =
      Hashtbl.update tbl ~k:key ~f:(fun _key value ->
      match Option.get_exn value with
      | 1 -> None
      | n -> Some (n - 1))
  in
  let sum = a + b in
  let inner_tbl = Hashtbl.find state.sums sum in
  decr inner_tbl a;
  decr inner_tbl b;
  if Hashtbl.length inner_tbl = 0 then
    Hashtbl.remove state.sums sum

let update_sums_for_added_number state n =
  Array.iter (fun x ->
      if x <> n then incr_sum_ref state x n)
    state.buffer

let update_sums_for_removed_number state n =
  Array.iter (fun x ->
      if x <> n then decr_sum_ref state x n)
    state.buffer

let init_state gen =
  let buffer = Array.init size (fun _ -> Gen.get_exn gen) in
  let sums = Hashtbl.create (size * size) in
  let state = { buffer; position = 0; sums } in
  Array.iter (update_sums_for_added_number state) buffer;
  state

let step state n =
  let removed = state.buffer.(state.position) in
  update_sums_for_removed_number state removed;
  state.buffer.(state.position) <- n;
  state.position <- (state.position + 1) mod size;
  update_sums_for_added_number state n


let has_sum state n =
  Hashtbl.get state.sums n
  |> Option.is_some

let find_invalid_number gen =
  let state = init_state gen in
  Gen.drop_while (fun n ->
      if has_sum state n then (
        step state n;
        true
      ) else
        false)
    gen
  |> Gen.next

let task1 input =
  numbers_gen input
  |> find_invalid_number
  |> Report.opt_int

let find_subarray_with_sum numbers sum =
  let prefixes = Hashtbl.create 64 in
  Hashtbl.add prefixes 0 (-1);
  let so_far = ref 0 in
  let exception Found of (int * int) in
  try
    for i = 0 to Array.length numbers - 1 do
      so_far := !so_far + numbers.(i);
      match Hashtbl.get prefixes (!so_far - sum) with
      | None -> Hashtbl.add prefixes !so_far i
      | Some prefix_end -> raise (Found (prefix_end + 1, i))
    done;
    None
  with Found pair ->
    Some pair

let min_max_in_array array low high =
  let min = ref Int.max_int in
  let max = ref Int.min_int in
  for i = low to high do
    min := Int.min !min array.(i);
    max := Int.max !max array.(i)
  done;
  (!min, !max)

let task2 input =
  let numbers = Gen.to_array (numbers_gen input) in
  Gen.of_array numbers
  |> find_invalid_number
  |> Option.get_exn
  |>  find_subarray_with_sum numbers
  |> Option.map (fun (low, high) ->
      let (min, max) = min_max_in_array numbers low high in
      min + max)
  |> Report.opt_int

let solutions = [| task1; task2 |]
