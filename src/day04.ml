open Containers

module String_map = Map.Make(String)

let in_range min max str =
  match int_of_string_opt str with
  | Some n when n >= min && n <= max -> true
  | None | Some _ -> false

let hex_char = function
  | 'a' .. 'f' | '0' .. '9' -> true
  | _ -> false

let byr_valid x = in_range 1920 2002 x

let iyr_valid x = in_range 2010 2020 x

let eyr_valid x = in_range 2020 2030 x

let hgt_valid x =
  (String.suffix ~suf:"cm" x
   && in_range 150 193 (String.sub x 0 (String.length x - 2)))
  ||
  (String.suffix ~suf:"in" x
   && in_range 59 76 (String.sub x 0 (String.length x - 2)))

let hcl_valid x =
  String.length x = 7
  && Char.equal x.[0] '#'
  && hex_char x.[1]
  && hex_char x.[2]
  && hex_char x.[3]
  && hex_char x.[4]
  && hex_char x.[5]
  && hex_char x.[6]

let ecl_valid = function
  | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
  | _ -> false

let pid_valid x = String.length x = 9 && String.for_all CCParse.is_num x

let cid_valid _ = true

let keys = [|
  ("byr", byr_valid);
  ("iyr", iyr_valid);
  ("eyr", eyr_valid);
  ("hgt", hgt_valid);
  ("hcl", hcl_valid);
  ("ecl", ecl_valid);
  ("pid", pid_valid);
|]

let record_string_to_string_map record_string =
  CCString.split_on_char ' ' record_string
  |> List.map (CCString.split_on_char ':')
  |> List.map (function | [k; v] -> (k, v) | _ -> failwith "unexpected record formar")
  |> String_map.of_list

let has_required_keys record =
  Array.for_all (fun (key, _) -> String_map.mem key record) keys

let is_valid record =
  Array.for_all (fun (key, valid) ->
      match String_map.find_opt key record with
      | Some str -> valid str
      | None -> false)
    keys

let validate predicate input =
  Utils.read_blocks_gen input
  |> Gen.map record_string_to_string_map
  |> Gen.filter predicate
  |> Gen.length
  |> Report.int

let task1 = validate has_required_keys

let task2 = validate is_valid

let solutions = [| task1; task2 |]
