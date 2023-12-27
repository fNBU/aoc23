import Lean.Data.Parsec
import Lean.Data.HashMap

import «Aoc23».List
import «Aoc23».Parsec
import «Aoc23».Option
import «Aoc23».HashMap

open Lean Lean.Parsec ParseResult Parsec HashMap

structure Card where
  n : Nat
  winning : List Nat
  had : List Nat
  deriving Repr

def Card.worth (c : Card) : Nat :=
    List.intersection c.winning c.had
    |>.length
    |> λ x =>
      match x with
      | 0 => 0
      | _ => Nat.pow 2 ( x - 1 )

def Card.worth2 (c : Card) : Nat :=
    List.intersection c.winning c.had
    |>.length

def Cards : Type := List Card

def Cards.worth (c : Cards) : Nat := c.map Card.worth |>.fold (.+.) ( 0 : Nat )

def parseOneCard : Parsec Card := do
  let _ ← skipString "Card"
  let _ ← spaces
  let n ← natDigits
  let _ ← skipString ": "
  let l ← manyNats
  let _ ← skipString " | "
  let m ← manyNats
  pure ({ n := n, winning := l, had := m } : Card)

def parseManyCards : Parsec $ List Card := do
  let a ← many $ optionalPrefixDiscard (manySingleChar '\n') parseOneCard
  return a.toList

def oneString (x : List String) : String :=
  x |> List.map String.data |> List.intercalate "\n".data |> String.mk

def day4_1 (input : List String)  :=
    Except.toOption $ Nat.repr <$> Cards.worth <$> Parsec.run parseManyCards (oneString input)

structure CardInstances where
  instances : Nat
  card : Card
  deriving Repr

abbrev CardsInstances := HashMap Nat CardInstances

def cardToInstances (c : Card) : CardInstances := { instances := 1, card := c }

def cardsInstances (c : Cards) : CardsInstances :=
  HashMap.ofList ( c.map (λ k =>  ( k.n , cardToInstances k ) ) )

def incrementBy (inc : Nat) (c : CardInstances) : CardInstances :=
  { instances := c.instances + inc, card := c.card }

def getWorth2 (c : CardsInstances) (i : Nat) : Option Nat :=
  match c.find? i with
  | none => none
  | some card => some $ Card.worth2 $ card.card

def day4_2 (input : List String) : Option String :=
  let cardsmap := cardsInstances <$> Parsec.run parseManyCards (oneString input)
  let indices := List.sortR (.<.) <$> HashMap.indices <$> cardsmap
  let rec helper (c : CardsInstances) (remainingIndices : List Nat) (accum : Nat) : Option Nat := do
    match remainingIndices with
    | [] => some accum
    | n::t =>
      let card ← c.find? n
      let instances ← card.instances
      let value ← getWorth2 c n
      let l := List.init value (λ x => x + n + 1)
      let c' ← mapAtList c l (incrementBy instances)
      helper c' t (accum + instances)
  do Nat.repr <$> (helper (←cardsmap |>.toOption) (←indices |>.toOption) 0)
