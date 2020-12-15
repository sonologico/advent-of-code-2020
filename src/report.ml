let int = Printf.printf "%d\n"

let int64 x = print_endline (Int64.to_string x)

let opt_int = function
  | None -> print_endline "Found no answer"
  | Some x -> int x
