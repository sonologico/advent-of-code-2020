open Containers

(* Asssumes:
 *   - At least one line
 *   - Lines of the same length
 *   - Lines of length at least one element
 *)
let count_trees hskip vskip grid =
  let line_length = String.length grid.(0) in
  let tree_count = ref 0 in
  let index = ref 0 in
  let line = ref 0 in
  while !line < Array.length grid do
    if Char.equal '#' grid.(!line).[!index] then
      incr tree_count;
    index := (!index + hskip) mod line_length;
    line := !line + vskip
  done;
  !tree_count

let task1 input =
  count_trees 3 1 (Utils.read_lines_array input)
  |> Report.int

let task2 input =
  let grid =  (Utils.read_lines_array input) in
  [
    (1, 1);
    (3, 1);
    (5, 1);
    (7, 1);
    (1, 2);
  ]
  |> List.fold_left (fun acc (hskip, vskip) ->
      acc * count_trees hskip vskip grid
    ) 1
  |> Report.int
