let rec fold_lines f acc input =
  match CCIO.read_line input with
  | None -> acc
  | Some line -> fold_lines f (f acc line) input

let read_lines_array input =
  CCIO.read_lines_gen input |> Gen.to_array

let read_blocks_gen input =
  let rec f buffer input () =
    match CCIO.read_line input, Buffer.length buffer with
    | None, 0 -> None
    | None, _ | Some "", _ ->
      let block = Buffer.contents buffer in
      Buffer.reset buffer;
      Some block
    | Some line, 0 ->
      Buffer.add_string buffer line;
      f buffer input ()
    | Some line, _ ->
      Buffer.add_char buffer ' ';
      Buffer.add_string buffer line;
      f buffer input ()
  in
  f (Buffer.create 4096) input
