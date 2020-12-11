let int = Printf.printf "%d\n"

let opt_int = function
  | None -> print_endline "Found no answer"
  | Some x -> int x
