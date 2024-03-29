import «Aoc23»

def maxFuel := 10000

def getLines (fuel : Nat) (accum : List String) (stream : IO.FS.Stream) : IO (Option (List String) ) := do
  match fuel with
  | 0 =>
    let stderr ← IO.getStderr
    stderr.putStrLn s!"Input file is too large. Stopped reading after {maxFuel} lines."
    pure none
  | n + 1 =>
    let buf ← stream.getLine
    if buf == "" then
      accum |>.reverse |> some |> pure
    else
      getLines n ( (String.dropRightWhile buf (· == '\n') ) :: accum) stream

-- From https://lean-lang.org/functional_programming_in_lean/hello-world/cat.html
def fileStream (filename : System.FilePath) : IO (Option IO.FS.Stream) := do
  let fileExists ← filename.pathExists
  if not fileExists then
    let stderr ← IO.getStderr
    stderr.putStrLn s!"File not found: {filename}"
    pure none
  else
    let handle ← IO.FS.Handle.mk filename IO.FS.Mode.read
    handle |> IO.FS.Stream.ofHandle |> some |> pure

def usage := "Usage:
aoc23 [   day1.1 | day1.2
        | day2.1 | day2.2
        | day3.1 | day3.2
        | day4.1 | day4.2
        | day5.1 | day5.2
        | day6.1 | day6.2
        | day7.1 | day7.2
        | day8.1 | day8.2
        | day9.1 | day9.2 ] FILE

Compute the solution of the Advent of Code 2023 problem on input FILE."

def outputSolution (a : Ex String) : IO UInt32 := do
  let stout ← IO.getStdout
  match a with
  | Except.error s =>
    stout.putStrLn s!"Failed to compute an answer. Message: {s}.}"
    pure 0
  | Except.ok s =>
    stout.putStrLn s!"Computed an answer: {s}."
    pure 0


def process (day : String) (path : String) : IO UInt32 := do
  let stream ← fileStream ⟨path⟩
  match stream with
  | none => pure 1
  | some stream =>
    let lines <- (getLines maxFuel [] stream)
    match lines with
    | none => pure 1
    | some l =>
      match day with
      | "day1.1" => outputSolution (day1_1 l)
      | "day1.2" => outputSolution (day1_2 l)
      | "day2.1" => outputSolution (Day2.day2_1 l)
      | "day2.2" => outputSolution (Day2.day2_2 l)
      | "day3.1" => outputSolution (day3_1 l)
      | "day3.2" => outputSolution (day3_2 l)
      | "day4.1" => outputSolution (Day4.day4_1 l)
      | "day4.2" => outputSolution (Day4.day4_2 l)
      | "day5.1" => outputSolution (Day5.day5_1 l)
      | "day5.2" => outputSolution (Day5.day5_2 l)
      | "day6.1" => outputSolution (day6_1 l)
      | "day6.2" => outputSolution (day6_2 l)
      | "day7.1" => outputSolution (day7_1 l)
      | "day7.2" => outputSolution (day7_2 l)
      | "day8.1" => outputSolution (day8_1 l)
      | "day8.2" => outputSolution (day8_2 l)
      | "day9.1" => outputSolution (day9_1 l)
      | "day9.2" => outputSolution (day9_2 l)
      | _ =>
        let stderr ← IO.getStderr
        stderr.putStrLn s!"Incorrect first argument: {day}."
        stderr.putStrLn usage
        pure 1

def main (args : List String) : IO UInt32 := do
  match args with
  | day :: path :: [] => process (day := day) (path := path)
  | _ =>
    let stderr ← IO.getStderr
    stderr.putStrLn s!"Wrong number of arguments."
    stderr.putStrLn usage
    pure 1
