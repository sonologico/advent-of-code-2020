module type Day = sig
  val task1 : in_channel -> unit
  val task2 : in_channel -> unit
end

let array : (module Day) array = [|
  (module Day01);
  (module Day02);
  (module Day03);
  (module Day04);
  (module Day05);
  (module Day06);
  (module Day07);
  (module Day08);
  (module Day09);
  (module Day10);
  (module Day11);
|]

let find day task =
  let (module D) = array.(day - 1) in
  match task with
  | 1 -> D.task1
  | 2 -> D.task2
  | _ -> raise Not_found
