import Mathlib.Util.Superscript -- provides (instance : Hashable Char) -- wtf??

import «Aoc23».Parsec
import «Aoc23».Util
import «Aoc23».Except
import «Aoc23».List
import «Aoc23».HashMap

open Lean Parsec' Except

inductive Instruction where
  | left
  | right
  deriving Inhabited, Repr, BEq, Hashable

open Instruction

def parseInstruction : Parsec' Instruction :=
  (pchar 'L' *> pure left) <|> (pchar 'R' *> pure right)

def Instructions := List Instruction
  deriving Inhabited, Repr, BEq, Hashable

def parseInstructions : Parsec' Instructions :=
  Array.toList <$> many parseInstruction

namespace Instructions

  def get (i : Instructions) (n : Nat) : Instruction :=
    i.get! (n % i.length)

end Instructions

def NodeLabel := Char × Char × Char
  deriving Inhabited, Repr, BEq, Hashable

namespace NodeLabel

  def final (n : NodeLabel) : Char :=
    match n with
    | (_, _, c) => c

end NodeLabel

def parseNodeLabel : Parsec' NodeLabel := do
  let a ← anyChar
  let b ← anyChar
  let c ← anyChar
  pure (a, b, c)

structure LR where
  left : NodeLabel
  right : NodeLabel
  deriving Inhabited, Repr, BEq, Hashable

structure Node where
  value : NodeLabel
  to : LR
  deriving Inhabited, Repr, BEq, Hashable

def parseNode : Parsec' Node := do
  let value ← parseNodeLabel
  let _ ← pstring " = ("
  let left ← parseNodeLabel
  let _ ← pstring ", "
  let right ← parseNodeLabel
  let _ ← pstring ")"
  pure { value := value, to := { left := left, right := right } }

structure Maps where
  instructions : Instructions
  nodes : HashMap NodeLabel LR
  deriving Inhabited, Repr

def parseMaps : Parsec' Maps := do
  let instructions ← parseInstructions
  let _ ← pstring "\n\n"
  let nodes ← HashMap.ofList <$> List.map (λ n => (n.value , n.to)) <$> Array.toList <$> sepBy parseNode (pchar '\n')
  let _ ← eof
  pure { instructions := instructions, nodes := nodes }

structure PointedMaps where
  instruction : Nat
  point : NodeLabel
  maps : Maps
  deriving Inhabited, Repr

def Maps.toPointedMaps (m : Maps) : PointedMaps :=
  { instruction := 0 , point := ('A','A','A'), maps := m }

namespace PointedMaps

  def next (m : PointedMaps) : Ex PointedMaps :=
    let nextPoint := m.maps.nodes.find? m.point
    match nextPoint with
    | none => error "PointedMaps.next: no entry for {m.point}"
    | some nextPoint =>
      match m.maps.instructions.get m.instruction with
      | left  => pure { m with instruction := m.instruction + 1, point := nextPoint.left  }
      | right => pure { m with instruction := m.instruction + 1, point := nextPoint.right }

  def final (m : PointedMaps) : Ex PointedMaps :=
    let rec helper (m : PointedMaps) (fuel : Nat): Ex PointedMaps :=
    open Nat in
    match fuel , m.next with
    | 0 , _ => error "g: fuel exhausted"
    | _ , error e => error e
    | n + 1 , ok m =>
      match m.point with
      | ('Z','Z','Z') => ok m
      | _ => helper m n
    helper m 100000

end PointedMaps

def day8_1 (input : List String) : Ex String :=
  Nat.repr
  <$> (λ x => x.instruction)
  <$> (
    Maps.toPointedMaps
    <$> parseMaps.run (oneString input)
    >>= PointedMaps.final
  )

structure PointsEdMaps where
  instruction : Nat
  points : List NodeLabel
  maps : Maps
  deriving Inhabited, Repr

def Maps.PointsEdMaps (m : Maps) : PointsEdMaps :=
  { instruction := 0 , points := m.nodes.toList |>.map (λ x => x.fst ) |>.filter (λ x => x.final == 'A' ), maps := m }

namespace PointsEdMaps

  def next (m : PointsEdMaps) : Ex PointsEdMaps :=
    let nextPoints := m.points.map (λ n => m.maps.nodes.find? n) |> List.mapM id
    match nextPoints with
    | none => error "PointedMaps.next: no entry for {m.points}"
    | some nextPoints =>
      match m.maps.instructions.get m.instruction with
      | left  => pure { m with instruction := m.instruction + 1, points := nextPoints.map (λ n => n.left)  }
      | right => pure { m with instruction := m.instruction + 1, points := nextPoints.map (λ n => n.right)  }

  def periods (m : PointsEdMaps) : Ex $ List Nat :=
    let rec helper (m : PointsEdMaps) (periods : List Nat) (fuel : Nat): Ex $ List Nat :=
    open Nat in
    match fuel , m.points with
    | zero , _ => error "g: fuel exhausted"
    | _ , [] => ok periods
    | succ n , _ =>
      let m := m.next
      match m with
      | error e => error e
      | ok m =>
        match m.points.any (λ p => p.final == 'Z') with
        | false => helper m periods n
        | true  => helper { m with points := m.points.filter (λ p => p.final != 'Z') } (m.instruction :: periods) n
    helper m [] 100000

end PointsEdMaps

def day8_2 (input : List String) : Ex String :=
  Nat.repr
  <$> List.fold Nat.lcm 1
  <$> (
    Maps.PointsEdMaps
    <$> parseMaps.run (oneString input)
    >>= PointsEdMaps.periods
  )
