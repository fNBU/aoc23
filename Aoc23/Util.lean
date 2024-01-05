import «Aoc23».Parsec
import «Aoc23».Except


def oneString (x : List String) : String :=
  x |> List.map String.data |> List.intercalate "\n".data |> String.mk

abbrev Ex α := Except String α

def Parsec'.failNext : Parsec' Unit := λ it => fail "" it.next

def getIt (s : Lean.Parsec.ParseResult α) : String.Iterator :=
  match s with
  | Lean.Parsec.ParseResult.success rem _ => rem
  | Lean.Parsec.ParseResult.error pos _ => pos
