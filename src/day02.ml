open Containers

type case = {
  low : int;
  high : int;
  char : char;
  password : string;
}

let read_case line =
  Scanf.sscanf line "%d-%d %c: %s" (fun low high char password ->
      { low; high; char; password; })

let count_char char string =
  String.fold
    (fun count ch ->
       if Char.equal ch char then
         count + 1
       else
         count)
    0 string

let case_is_valid_count {low; high; char; password} =
  let count = count_char char password in
  count >= low && count <= high

let case_is_valid_position { low; high; char; password } =
  let char_in_pos pos =
    String.length password >= pos && Char.equal char password.[pos - 1]
  in
  let low_has_char = char_in_pos low in
  let high_has_char = char_in_pos high in
  (low_has_char || high_has_char) && not (low_has_char && high_has_char)

let report_valid_count predicate input =
  CCIO.read_lines_gen input
  |> Gen.map read_case
  |> Gen.filter predicate
  |> Gen.length
  |> Report.int

let task1 = report_valid_count case_is_valid_count

let task2 = report_valid_count case_is_valid_position

let solutions = [| task1; task2 |]
