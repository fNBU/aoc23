import Mathlib.Util.Superscript -- provides (instance : Hashable Char) -- wtf??

import «Aoc23».List

open Lean
open List

structure Location where
  row : Nat
  column : Nat
  deriving Repr, BEq

def Location.adjacent ( l : Location ) : List Location :=
  let all := [
      { l with row := l.row + 1 } -- north
    , { l with row := l.row - 1 } -- south
    , { l with column := l.column + 1 } -- east
    , { l with column := l.column - 1 } -- west
    , { l with row := l.row + 1 , column := l.column + 1 } -- northeast
    , { l with row := l.row + 1 , column := l.column - 1 } -- northwest
    , { l with row := l.row - 1 , column := l.column + 1 } -- southeast
    , { l with row := l.row - 1 , column := l.column - 1 } -- southwest
  ]
  List.diff all [l] |> List.distinct

structure Extent where
  oneEnd : Location
  otherEnd : Location
  deriving Repr, BEq

def between (e : Extent) : List Location :=
  let width := 1 + Int.natAbs (Int.ofNat e.oneEnd.column - Int.ofNat e.otherEnd.column)
  let left := if e.oneEnd.column < e.otherEnd.column then e.oneEnd else e.otherEnd
  List.init width (fun c => {left with column := (left.column + c)})

inductive Component where
  | number (raw : String) (ext : Extent)
  | symbol (raw : Char) (loc : Location)
  deriving Repr, BEq

def Component.toInt ( c : Component ) : Option Int := ( match c with | Component.symbol _ _ => "0" | Component.number x _ => x ) |> String.toInt?

def Component.locationOrExtent ( x : Component ) : List Location :=
  match x with
    | Component.symbol _ l => [ l ]
    | Component.number _ e => e |> between

def Component.adjacent ( x : Component ) : List Location :=
  match x with
    | Component.symbol _ l => l |> Location.adjacent
    | Component.number _ e =>
      let btw := e |> between
      let all := btw |> List.map Location.adjacent |> List.join
      List.diff all btw |> List.distinct

def Component.adjacentQ ( x : Component ) ( y : Component ) : Bool :=
  let xLoc := x |> Component.locationOrExtent
  let yAdj := y |> Component.adjacent
  List.intersection xLoc yAdj |> List.isEmpty |> Bool.not

abbrev ComponentSet := List Component

def ComponentSet.adjacentComponents (c : Component) (cs : ComponentSet) : ComponentSet :=
  cs |> filter ( Component.adjacentQ c )

def consumeAny ( headOfList : Location ) ( cs : ComponentSet ) ( digitAccum : String ) ( l : List Char ) : Option ComponentSet :=
  let maybeNewNumber :=
    Component.number
    digitAccum
    {
      oneEnd := { headOfList with column := headOfList.column - digitAccum.length } ,
      otherEnd := { headOfList with column := headOfList.column - 1 }
    }

  let oneToTheRight := { headOfList with column := headOfList.column + 1 }

  let newCS :=
    match digitAccum with
      | "" => cs
      | _ => ( maybeNewNumber :: cs )

  match l with
    | [] => some newCS
    | 'n' :: 'e' :: 'w' :: 'l' :: 'i' :: 'n' :: 'e' :: ys => consumeAny { headOfList with row := headOfList.row + 1 , column := 0 } newCS "" ys
    | '.' :: ys => consumeAny oneToTheRight newCS "" ys
    | y :: ys =>
      match y.isDigit with
        | false => consumeAny oneToTheRight ( ( Component.symbol y headOfList ) :: newCS ) "" ys
        | true => consumeAny oneToTheRight cs ( digitAccum ++ y.toString ) ys

def Component.isNumber (x : Component) : Bool :=
  match x with
    | Component.number _ _ => true
    | _ => false

def Component.isSymbol (x : Component) : Bool :=
  match x with
    | Component.symbol _ _ => true
    | _ => false

def oneList (x : List String) : List Char :=
  x |> List.map String.data |> List.intercalate "newline".data

def allComponents (input : List String) : Option ComponentSet := input |> oneList |> consumeAny ⟨0,0⟩ empty ""

def day3_1 (input : List String) : Option String :=
  let filterCS ( cs : ComponentSet ) : ComponentSet :=
    cs |> filter ( fun c => Component.isNumber c && any ( ComponentSet.adjacentComponents c cs ) Component.isSymbol )

  Int.repr <$> fold Int.add 0 <$> filterOption <$> map Component.toInt <$> filterCS <$> allComponents input

def day3_2 (input : List String) : Option String :=
  let Component.isAsterisk ( x : Component ) : Bool :=
    match x with
      | Component.symbol '*' _ => true
      | _ => false

  let twoNumberNeighbors ( c : Component ) ( cs : ComponentSet ) : Option ( List Int ) :=
    let n := ComponentSet.adjacentComponents c cs
    let t := n |> filter Component.isNumber |> List.map Component.toInt |> List.filterOption
    match Component.isAsterisk c , t.length with
      | true , 2 => some t
      | _ , _ => none

  let getNumberPairs ( cs : ComponentSet ) : List ( List Int ) :=
    cs |> map ( fun c => twoNumberNeighbors c cs ) |> filterOption

  Int.repr <$> fold Int.add 0 <$> map (List.fold Int.mul 1) <$> getNumberPairs <$> allComponents input
