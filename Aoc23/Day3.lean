import Mathlib.Util.Superscript -- provides (instance : Hashable Char) -- wtf??

import «Aoc23».List
import «Aoc23».Array
import «Aoc23».Util
import «Aoc23».Except
import «Aoc23».Parsec

open Lean Parsec.ParseResult Parsec' Except

structure Location where
  row : Nat
  column : Nat
  deriving Repr, BEq, Hashable, Inhabited

namespace Location

  def adjacent ( l : Location ) : List Location :=
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

end Location

structure Extent where
  oneEnd : Location
  otherEnd : Location
  deriving Repr, BEq, Hashable, Inhabited

namespace Extent

  /--
  Returns a list of all `location`s between the two ends of the extent, inclusive. Does NOT check that the ends are in the same row (uses the row of the leftmost cell).
  -/
  def between! (e : Extent) : List Location :=
    let width := 1 + Int.natAbs (Int.ofNat e.oneEnd.column - Int.ofNat e.otherEnd.column)
    let left := if e.oneEnd.column < e.otherEnd.column then e.oneEnd else e.otherEnd
    List.init width (fun c => {left with column := (left.column + c)})

  /--
  Returns a list of all `location`s between the two ends of the extent, inclusive. Returns `Except.error` if the ends are not in the same row.
  -/
  def between (e : Extent) : Ex (List Location) :=
    if e.oneEnd.row ≠ e.otherEnd.row then error "Extent.between: ends are not in the same row"
    else ok (e.between!)

end Extent

inductive Component where
  | number (raw : Nat) (ext : Extent)
  | symbol (raw : Char) (loc : Location)
  deriving Repr, BEq, Hashable, Inhabited

namespace Component

  def isNumber (x : Component) : Bool :=
    match x with
      | number _ _ => true
      | _ => false

  def isSymbol (x : Component) : Bool :=
    match x with
      | symbol _ _ => true
      | _ => false

  def toNat ( c : Component ) : Ex Nat :=
    match c with
    | symbol _ _ => ok 0
    | number x _ => ok x

  def locationOrExtent ( x : Component ) : List Location :=
    match x with
      | symbol _ l => [ l ]
      | number _ e => e.between!

  def adjacent ( x : Component ) : List Location :=
    match x with
      | symbol _ l => l.adjacent
      | number _ e =>
        let btw := e.between!
        let all := btw |>.map Location.adjacent |>.join
        all.diff btw |>.distinct

  def adjacentQ ( x : Component ) ( y : Component ) : Bool :=
    let xLoc := x |>.locationOrExtent
    let yAdj := y |>.adjacent
    xLoc.intersection yAdj |>.isEmpty |>.not

end Component

open Component

def parseComponent ( posToLoc : Nat -> Location ) : Parsec' Component := λ it =>
  match it.atEnd , it.curr with
  | true  , _    => error it "parseComponent: input was empty"
  | false , '.'  => error it "parseComponent: input was a period"
  | false , '\n' => error it "parseComponent: input was a newline"
  | false , x    =>
    match x.isDigit with
    | false => success it.next ( symbol x ( posToLoc it.i.byteIdx ) )
    | true =>
      match nat it with
      | Parsec.ParseResult.error it' err => error it' err
      | success it' digits =>
        let ext := { oneEnd := posToLoc it.i.byteIdx , otherEnd := posToLoc ( it'.i.byteIdx - 1 ) }
        success it' ( number digits ext )

abbrev ComponentSet := List Component

def parseComponentSet : Parsec' ComponentSet := λ it =>
  let columns := it.s.takeWhile (fun c => c ≠ '\n') |>.length

  let posToLocation ( p : Nat ) : Location :=
    let r := p / ( 1 + columns )
    let c := p % ( 1 + columns )
    { row := r , column := c }

  let skip := manyCharsList [ '.' , '\n' ]

  ( Array.toList <$> ( skip *> ( sepBy (parseComponent posToLocation) skip ) <* skip <* eof ) ) it

def ComponentSet.adjacentComponents (cs : ComponentSet) (c : Component) : ComponentSet :=
  cs |>.filter ( Component.adjacentQ c )

def assemble ( ns : List Nat ) : String :=
  ns.fold (.+.) 0 |>.repr

def day3_1 (input : List String) : Ex String :=
  let filterCS ( cs : ComponentSet ) : ComponentSet :=
    let symbols : ComponentSet := cs |>.filter Component.isSymbol
    cs |>.filter ( fun c => Component.isNumber c && ( symbols.adjacentComponents c  |>.isEmpty |> not ) )

  let getNums ( cs : ComponentSet ) : List Nat :=
    cs |>.map Component.toNat |>.filterExcept

  assemble <$> getNums <$> filterCS <$> parseComponentSet.run (oneString input)

def day3_2 (input : List String) : Ex String :=
  let Component.isAsterisk ( x : Component ) : Bool :=
    match x with
      | Component.symbol '*' _ => true
      | _ => false

  let twoNumberNeighbors ( c : Component ) ( cs : ComponentSet ) : Ex ( List Nat ) :=
    let n := cs.adjacentComponents c
    let t := n |>.filter Component.isNumber |>.map Component.toNat |>.filterExcept
    match Component.isAsterisk c , t.toList.length with
      | true , 2 => ok t.toList
      | _ , _ => error "twoNumberNeighbors: Didn't find two neighbors"

  let getNumberPairs ( cs : ComponentSet ) : List ( List Nat ) :=
    cs |>.map ( fun c => twoNumberNeighbors c cs ) |>.filterExcept

  assemble <$> List.map (List.fold (.*.) 1) <$> getNumberPairs <$> parseComponentSet.run (oneString input)
