import «Aoc23».List
import «Aoc23».Parsec
import «Aoc23».Option
import «Aoc23».HashMap
import «Aoc23».Util
import «Aoc23».Except

open Except

open Parsec' Lean.Parsec.ParseResult HashMap

namespace Day5

structure Map where
  destination : Nat
  source : Nat
  rangeLength : Nat
  deriving Repr, BEq, Inhabited

def parseOneMap : Parsec' Map := do
  let destination ← nat
  let _ ← manySingleChar ' '
  let source ←  nat
  let _ ← manySingleChar ' '
  let rangeLength ←  nat
  return ({ destination := destination, source := source, rangeLength := rangeLength } : Map)

inductive Category where
  | fertilizer : Category
  | humidity : Category
  | light : Category
  | location : Category
  | seed : Category
  | soil : Category
  | temperature : Category
  | water : Category
  deriving Repr, BEq, Inhabited

def parseOneCategory : Parsec' Category := do
  let n ← manyChars asciiLetter
  match n with
  | "fertilizer" => return Category.fertilizer
  | "humidity" => return Category.humidity
  | "light" => return Category.light
  | "location" => return Category.location
  | "seed" => return Category.seed
  | "soil" => return Category.soil
  | "temperature" => return Category.temperature
  | "water" => return Category.water
  | _ => fail "parseOneCategory: unknown category"

structure Maps where
  source : Category
  destination : Category
  maps : List Map
  deriving Repr, BEq, Inhabited

def parseMaps : Parsec' Maps := do
  let source ← parseOneCategory
  let _ ← pstring "-to-"
  let destination ← parseOneCategory
  let _ ← pstring " map:"
  let _ ← pchar '\n'
  let maps ← sepBy parseOneMap (pchar '\n')
  return ({ source := source, destination := destination, maps := maps.toList } : Maps)

structure Almanac where
  seeds : List Nat
  mapss : List Maps
  deriving Repr, BEq, Inhabited

def parseAlminac : Parsec' Almanac := do
  let _ ← pstring "seeds:"
  let _ ← ws
  let seeds ← sepBy nat (manySingleChar ' ')
  let _ ← manySingleChar '\n'
  let mapss ← sepBy parseMaps (manySingleChar '\n')
  return ({ seeds := seeds.toList, mapss := mapss.toList } : Almanac)

structure Value where
  value : Nat
  category : Category
  deriving Repr, BEq, Inhabited

def Map.toFunction (m : Map) (x : Nat) : Nat :=
  if
    x >= m.source && x < m.source + m.rangeLength
  then
    m.destination + (x - m.source)
  else
    x

def Maps.find (m : Maps) (x : Nat) : Option Map :=
  m.maps.find? (λ m => x >= m.source && x < m.source + m.rangeLength)

def Maps.toFunction (m : Maps) (x : Value) : Option Value :=
  if
    x.category == m.source
  then
    match m.find x.value with
    | none => some { x with category := m.destination }
    | some map => some { category := m.destination , value := map.toFunction x.value }
  else
    none

def Mapss.once (m : List Maps) : Value -> Value := λ x =>
  let f (x : Value × Bool) (m : Maps) :=
    let hit? := x.snd
    match hit? with
    | true => (x.fst, true)
    | false =>
      match m.toFunction x.fst with
      | none => (x.fst , false)
      | some v' => (v' , true)
  m.foldl f (x, false) |> Prod.fst

partial def Mapss.orbit (m : List Maps) (v : Value) : List Value :=
  let rec helper (m : List Maps) (v : Value) (accum : List Value) : List Value :=
    let v' := Mapss.once m v
    if v' == v then
      accum
    else
      helper m v' (v' :: accum)
  helper m v [v] |>.reverse

def Almanac.orbits (a : Almanac) : List $ List Value :=
  a.seeds.map (λ s => Mapss.orbit a.mapss { value := s, category := Category.seed })


def lowestLocation (l : List $ List Value) :=
  l.map (λ x => x.filter (λ x => x.category == Category.location))
  |>.flatten
  |>.sort (λ x y => x.value < y.value)
  |>.head?
  |>.toExcept "lowestLocation: Didn't find any elements with category equal to Category.location"
  |> Except.map (λ x => x.value)

def day5_1 (input : List String) : Ex String :=
  Nat.repr <$> ( Except.join $ lowestLocation <$> Almanac.orbits <$> ( Parsec'.run parseAlminac (oneString input) ) )

structure Range where
  start : Nat
  length : Nat
  deriving Repr, BEq, Inhabited

def parseRange : Parsec' Range := do
  let start ← nat
  let _ ← manySingleChar ' '
  let length ← nat
  return Range.mk start length


def Range.end (r : Range) : Nat := r.start + r.length - 1

namespace Range

  def mkEndStyle! (s : Nat) (e : Nat) : Range :=
      { start := s , length := e - s + 1 }

  def mkEndStyle (s : Nat) (e : Nat) : Option Range :=
    if e >= s then
      mkEndStyle! s e |> some
    else
      none

  def isEmpty : Range -> Bool := (. == 0) ∘ length

  def isNonEmpty : Range -> Bool := not ∘ isEmpty

  def overlap (x : Range) (y : Range) : Bool :=
    ( x.end >= y.start ) && ( y.end >= x.start )

  def abut (x : Range) (y : Range) : Bool :=
    ( x.end == y.start - 1 ) || ( y.end == x.start - 1)

  def touch (x : Range) (y : Range) : Bool :=
    x.overlap y || x.abut y

  def coalesce (x : Range) (y : Range) : Option Range :=
    if x.touch y then
      let s := Nat.min x.start y.start
      let e := Nat.max x.end y.end
      mkEndStyle s e
    else
      none

end Range

abbrev Ranges := List Range

namespace Ranges

  def addWithCoalesce (x : Ranges) (y : Range) : Ranges :=
    let rec helper (x : Ranges) (y : Range) (accum : Ranges) : Ranges :=
      match x with
      | [] => y :: accum
      | x :: xs =>
        match x.coalesce y with
        | none => helper xs y (x :: accum)
        | some z => List.join [ xs , [z] , accum ]
    helper x y [] |>.reverse

  def coalesce (l : Ranges) : Ranges :=
    let rec helper (l : Ranges) (accum : Ranges) : Ranges :=
      match l with
      | [] => accum
      | x :: xs => helper xs (addWithCoalesce accum x)
  helper l []

end Ranges

/-
  Split `y` over `x`. Not symmetric!! for example, if `y` is contained in `x`, then return a list with one element, `y`, but if `x` is properly contained in `y`, then at least two elements will be returned.
-/
def Range.splitOver (y : Range) (x : Range) : Ranges :=
  if x.overlap y then
    match ( ( y.start < x.start ) : Bool ) , ( ( x.end < y.end ) : Bool ) with
    | false , false => -- x.start <= y.start <= y.end <= x.end. y is contained in x
      [ y ]
    | true  , false => -- y.start < x.start , y.end <= x.end. y hangs over the left of x.
      [
        Range.mkEndStyle! y.start ( x.start - 1 ) ,
        Range.mkEndStyle! x.start y.end
      ]
    | false , true  => -- x.start <= y.start, x.end < y.end. y hangs over the right of x.
      [
        Range.mkEndStyle! y.start x.end ,
        Range.mkEndStyle! ( x.end + 1 ) y.end
      ]
    | true  , true  => -- y.start < x.start <= x.end < y.end. x is contained in y
      [
        Range.mkEndStyle! y.start ( x.start - 1 ) ,
        x ,
        Range.mkEndStyle! ( x.end + 1 ) y.end
      ]
  else
    [ y ]

structure Map' where
  destination : Range
  source : Range
  deriving Repr, BEq, Inhabited

def parseMap' : Parsec' Map' := do
  let destinationStart ← nat
  let _ ← manySingleChar ' '
  let sourceStart ← nat
  let _ ← manySingleChar ' '
  let length ← nat
  return {
    destination := Range.mk destinationStart length
    , source := Range.mk sourceStart length
  }

structure Maps' where
  source : Category
  destination : Category
  maps : List Map'
  deriving Repr, BEq, Inhabited

def parseMaps' : Parsec' Maps' := do
  let source ← parseOneCategory
  let _ ← pstring "-to-"
  let destination ← parseOneCategory
  let _ ← pstring " map:"
  let _ ← pchar '\n'
  let maps ← sepBy parseMap' (manySingleChar '\n')
  return { source := source, destination := destination, maps := maps.toList }


structure Almanac' where
  seeds : Ranges
  mapss : List Maps'
  deriving Repr, BEq, Inhabited

def parseAlminac' : Parsec' Almanac' := do
  let _ ← pstring "seeds:"
  let _ ← ws
  let seeds ← sepBy parseRange (manySingleChar ' ')
  let _ ← manySingleChar '\n'
  let mapss ← sepBy parseMaps' (many (pchar '\n'))
  return { seeds := seeds.toList , mapss := mapss.toList }

structure Value' where
  value : Ranges
  category : Category
  deriving Repr, BEq, Inhabited

def mapRange (m : Map') (r : Range) : Ranges :=
  if m.source.overlap r then
    match ( ( r.start < m.source.start ) : Bool ) , ( ( m.source.end < r.end ) : Bool ) with
    | false , false => -- m.source.start <= r.start <= r.end <= m.source.end. r is contained in m
      [
        Range.mk
          ( m.destination.start + ( r.start - m.source.start ) )
          r.length
      ]
    | true  , false => -- r.start < m.source.start , r.end <= m.source.end. r hangs over the left of m.
      [
        Range.mk
          r.start
          ( m.source.start - r.start ) ,
        Range.mk
          m.destination.start
          ( r.end - m.source.start + 1 )
      ] |> Ranges.coalesce
    | false , true  => -- m.source.start <= r.start, m.source.end < r.end. r hangs over the right of m.
      [
        Range.mk
          ( m.destination.start + ( r.start - m.source.start ) )
          ( m.source.end - r.start + 1 ) ,
        Range.mkEndStyle!
          ( m.source.end + 1 )
          r.end
      ] |> Ranges.coalesce
    | true  , true  => -- r.start < m.source.start <= m.source.end < r.end. m is contained in r
      [
        Range.mkEndStyle!
          r.start
          ( m.source.start - 1 ) ,
        m.destination ,
        Range.mkEndStyle!
          ( m.source.end + 1 )
          r.end
      ] |> Ranges.coalesce
  else
    [ r ]

def Map'.toFunction (m : Map') (x : Ranges) : Ranges :=
  x |>.map (mapRange m) |>.join

def mapsRanges (m : List Map') (x : Ranges) : Ranges :=
  let rec helper (m : List Map') (x : Ranges) (accum : Ranges) : Ranges :=
    match m with
    | [] => accum ++ x
    | m :: ms =>
      let rs := x.map (λ r => Range.splitOver r m.source) |>.join
      let mapped := rs.filter ( Range.overlap m.source ) |> m.toFunction
      let unmapped := rs.filter ( λ r => not ( m.source.overlap r ) )
      helper ms unmapped (accum ++ mapped)
  helper m x []

def Maps'.toFunction (m : Maps') (x : Value') : Option $ Value' :=
  if
    x.category == m.source
  then
    { value := mapsRanges m.maps x.value |>.coalesce , category := m.destination } |> some
  else
    none

def Maps's.once (m : List Maps') : Value' -> Value' := λ x =>
  let f (x : Value' × Bool) (m : Maps') :=
    let hit? := x.snd
    match hit? with
    | true => (x.fst, true)
    | false =>
      match m.toFunction x.fst with
      | none => (x.fst , false)
      | some v' => (v' , true)
  m.foldl f (x, false) |> Prod.fst

partial def Maps's.orbit (m : List Maps') (v : Value') : List Value' :=
  let rec helper (m : List Maps') (v : Value') (accum : List Value') : List Value' :=
    let v' := Maps's.once m v
    if v' == v then
      accum
    else
      helper m v' (v' :: accum)
  helper m v [v] |>.reverse

def Almanac'.orbits (a : Almanac') : List Value' :=
  Maps's.orbit a.mapss ( { value := a.seeds, category := Category.seed } : Value' )

def lowestLocation' (l : List Value') : Ex Nat :=
  l.filter (λ x => x.category == Category.location)
  |>.map (λ x => x.value)
  |>.flatten
  |>.map (λ x => x.start)
  |>.sort (λ x y => x < y)
  |>.head?
  |>.toExcept "lowestLocation': Didn't find any elements with category equal to Category.location"

def day5_2 (input : List String) : Ex String :=
  Nat.repr <$> ( Except.join $ lowestLocation' <$> Almanac'.orbits <$> ( Parsec'.run parseAlminac' (oneString input) ) )
