import «Aoc23».List
import «Aoc23».Util
import «Aoc23».Parsec
import «Aoc23».HashMap
import «Aoc23».Option

open Lean Parsec' Except HashMap

namespace Day4

structure Card where
  n : Nat
  winning : List Nat
  had : List Nat
  deriving Repr, BEq

def parseOneCard : Parsec Card := do
  let _ ← skipString "Card" *> ws
  let n ← nat
  let _ ← ws *> skipString ":" *> ws
  let l ← sepByL nat ws
  let _ ← ws *> skipString "|" *> ws
  let m ← sepByL nat spaces
  return ({ n := n, winning := l, had := m } : Card)

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

def parseCards : Parsec $ List Card := do
  let a ← sepBy parseOneCard (pchar '\n')
  return a.toList

def Cards.worth (c : Cards) : Nat := c.map Card.worth |>.fold (.+.) ( 0 : Nat )


def day4_1 (input : List String) : Ex String :=
    Nat.repr <$> Cards.worth <$> Parsec.run parseCards (oneString input)

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

def getWorth2 (c : CardsInstances) (i : Nat) : Ex Nat :=
  match c.find? i with
  | none => error s!"getWorth2: Didn't find a card with the given index {i}"
  | some card => ok $ Card.worth2 $ card.card

def day4_2 (input : List String) : Ex String :=
  let cardsmap := cardsInstances <$> Parsec.run parseCards (oneString input)
  let indices := List.sortR (.<.) <$> HashMap.indices <$> cardsmap
  let rec helper (c : CardsInstances) (remainingIndices : List Nat) (accum : Nat) : Ex Nat := do
    match remainingIndices with
    | [] => ok accum
    | n::t =>
      let card ← (c.find? n).toExcept s!"day4_2.helper: Didn't find a card with the given index {n}"
      let instances := card.instances
      let value ← getWorth2 c n
      let l := List.init value (λ x => x + n + 1)
      let c' := mapAtList c l (incrementBy instances)
      helper c' t (accum + instances)
  do Nat.repr <$> (helper (←cardsmap) (←indices) 0)
