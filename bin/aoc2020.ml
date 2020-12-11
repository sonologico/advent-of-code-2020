let error str =
  prerr_endline str;
  exit 1

let _ =
  match Sys.argv with
  | [| _; day; task; file |] ->
    let solution =
      try
        Aoc2020lib.Solutions.find (int_of_string day) (int_of_string task)
      with
      | _ -> error (Printf.sprintf "Invalid day and task: '%s' '%s'" day task)
    in
    (try CCIO.with_in file solution
     with Sys_error e -> error e)
  | _ -> error "Bad args"
