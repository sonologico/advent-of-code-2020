let array = [|
  Day01.solutions;
  Day02.solutions;
  Day03.solutions;
  Day04.solutions;
  Day05.solutions;
  Day06.solutions;
  Day07.solutions;
|]

let find day task =
  array.(day - 1).(task - 1)
