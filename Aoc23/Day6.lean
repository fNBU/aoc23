import «Aoc23».Parsec
import «Aoc23».Util
import «Aoc23».Except
import «Aoc23».Int
import «Aoc23».Float

open Except

open Parsec' Lean.Parsec.ParseResult

def Times := List Nat
  deriving Repr, Inhabited

def parseTimes : Parsec' Times := do
  let _ ← pstring "Time:"
  let _ ← ws
  let times ← manyNats
  return times

def Distances := List Nat
  deriving Repr, Inhabited

def parseDistances : Parsec' Distances := do
  let _ ← pstring "Distance:"
  let _ ← ws
  let distances ← manyNats
  return distances

structure Race where
  time : Nat
  distance : Nat
  deriving Repr, Inhabited

def Races := List Race
  deriving Repr, Inhabited

def parseRaces : Parsec' Races := do
  let times ← parseTimes
  let _ ← pchar '\n'
  let distances ← parseDistances
  return times.zip distances |>.map (λ (time, distance) => { time := time, distance := distance })

namespace Race

  def bounds (r : Race) : Float × Float :=
    let term1 := r.time.toFloat / 2.0
    let term2 := 0.5 * Float.sqrt ( r.time.toFloat * r.time.toFloat - 4.0 * r.distance.toFloat )
    ( term1 - term2 , term1 + term2)

  def winningBounds (r : Race) : Int × Int :=
    let (lb, ub) := r.bounds
    let (lbInt, _) := lb.intAndFrac
    let (ubInt, ubFrac) := ub.intAndFrac
    let lb' := lbInt + 1
    let ub' :=
      if ubFrac == 0.0 then
        ubInt - 1
      else
        ubInt
    ( lb' , ub' )

  def waysToWin (r : Race) : Int :=
    let (lb, ub) := r.winningBounds
    ub - lb + 1

end Race

def day6_1 (input : List String) :=
  Int.repr <$> List.foldl (.*.) 1 <$> List.map Race.waysToWin <$> parseRaces.run (oneString input)

def parseBadKerning (p : Parsec' String ): Parsec' Nat := do
  let _ ← p
  let _ ← ws
  let time ← sepBy (manyChars digit) (manySingleChar ' ')
  match time |>.toList |>.map String.data |>.join |> String.mk |>.toNat? with
  | some time => return time
  | none => fail "parseBadKerning: could not parse time as a Nat"

def parseRaceBadKerning : Parsec' Races := do
  let time ← parseBadKerning (pstring "Time:")
  let _ ← pchar '\n'
  let distance ← parseBadKerning (pstring "Distance:")
  return [{ time := time, distance := distance }]

def day6_2 (input : List String) : Ex String :=
  Int.repr <$> List.foldl (.*.) 1 <$> List.map Race.waysToWin <$> parseRaceBadKerning.run (oneString input)
