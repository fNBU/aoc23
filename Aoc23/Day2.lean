import «Aoc23».Util
import «Aoc23».Parsec

open Lean Parsec' Parsec.ParseResult

namespace Day2

/--
  ` red | green | blue : Color`
-/
inductive Color where
  | red
  | green
  | blue
  deriving Repr , BEq

open Color

def parseColor : Parsec Color := do
  let n ← manyChars asciiLetter
  match n with
  | "red"   => return red
  | "green" => return green
  | "blue"  => return blue
  | _ => fail "parseColor: unknown Color"

structure Reveal where
  color : Color
  number : Nat
  deriving Repr , BEq

def parseCard : Parsec Reveal := do
  let n <- nat
  let _ <- pchar ' '
  let c <- parseColor
  return { color := c , number := n }

structure Hand where
  hred   : Reveal
  hgreen : Reveal
  hblue  : Reveal
  deriving Repr, BEq

def parseHand : Parsec Hand := do
  let maybecards ←  sepBy parseCard (pstring ", ")
  let f (l : Array Reveal) (c : Color) : Option Reveal :=
    match l.filter (λ x => x.color == c) with
    | #[ x ] => some x
    | #[] => some { color := c , number := 0 }
    | _ => none
  match f maybecards red , f maybecards green , f maybecards blue with
  | some r , some g , some b => return { hred := r , hgreen := g , hblue := b }
  | _ , _ , _ => fail "parseHand: Didn't get one or zero occurrences of each color"

def Hand.max (a b : Hand) : Hand :=
  {
    hred   := { color := red   , number := Nat.max a.hred.number   b.hred.number   } ,
    hgreen := { color := green , number := Nat.max a.hgreen.number b.hgreen.number } ,
    hblue  := { color := blue  , number := Nat.max a.hblue.number  b.hblue.number  }
  }

structure Game where
  gameNumber : Nat
  hands : List Hand
  deriving Repr , BEq

def parseGame : Parsec Game := do
  let _ <- pstring "Game "
  let n ← nat
  let _ <- pstring ": "
  let hands ← sepBy parseHand (pstring "; ")
  return { gameNumber := n , hands := hands.toList }

abbrev Games := List Game

def parseGames : Parsec' Games := do
  let games ← sepBy parseGame (pchar '\n')
  return games.toList

def dealWithOneGame (g : Game) : Nat × Hand :=
  let h :=
    g.hands.foldl
      Hand.max
      { hred := ⟨ red , 0 ⟩ , hblue :=  ⟨ blue , 0 ⟩ , hgreen :=  ⟨ green , 0 ⟩ }
  ( g.gameNumber , h )

def redLimit := 12
def greenLimit := 13
def blueLimit := 14

def filterAndSum (x : Games) : String :=
  x
  |>.map dealWithOneGame
  |>.filter (λ x => x.snd.hred.number <= redLimit && x.snd.hgreen.number <= greenLimit && x.snd.hblue.number <= blueLimit)
  |>.foldl (λ x y => x + y.fst) 0
  |> Nat.repr

def day2_1 (input : List String) : Ex String :=
  filterAndSum <$> Parsec'.run parseGames (oneString input)

def productAndSum (x : Games) :=
  x
  |>.map dealWithOneGame
  |>.map (λ x => x.snd.hred.number * x.snd.hgreen.number * x.snd.hblue.number)
  |>.foldl (.+.) 0
  |> Nat.repr

def day2_2 (input : List String) : Ex String :=
  productAndSum <$> Parsec'.run parseGames (oneString input)
