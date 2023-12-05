import «Aoc23».Option
import «Aoc23».Prod

structure ColorCounts where
  red : Nat
  green : Nat
  blue : Nat
deriving Repr

def ColorCounts.max (a b : ColorCounts) : ColorCounts :=
  { red := Max.max a.red b.red , blue := Max.max a.blue b.blue , green := Max.max a.green b.green }

def mySplit (a : Char) (b : String) : List String := String.split b ( · == a )

def twoElementListToProd (x : List α) : Option (α × α) :=
  match x with
  | a :: b :: [] => some (a,b)
  | _ => none

def getGameNumber (s : String) : Option Nat :=
  match s.data with
  | 'G' :: 'a' :: 'm' :: 'e' :: ' ' :: as => some as.asString.toNat? |> Option.join
  | _ => none

def myDropWhile (a : Char) (b : String) : String := String.dropWhile b ( · == a )

def peelDigits (l : List Char) : ( List Char ) × ( List Char ) :=
  let rec helper (accum l : List Char) : List Char × List Char :=
    match l with
    | [] => ( accum.reverse , [] )
    | x :: xs => if x.isDigit then helper (x :: accum) xs else ( accum , l )
  helper [] l

mutual
  def getNum (x : List Char) (counts : ColorCounts) : Option (ColorCounts × List Char) :=
    match x with
      -- not very robust but avoids a termination proof
      | d1 :: d2 :: d3 :: ' ' :: xs => getOneColor xs counts (Option.getD [d1 , d2 , d3].asString.toNat? 0)
      | d1 :: d2       :: ' ' :: xs => getOneColor xs counts (Option.getD [d1 , d2].asString.toNat?      0)
      | d1             :: ' ' :: xs => getOneColor xs counts (Option.getD [d1].asString.toNat?           0)
      | xs => (counts , xs)

  def getOneColor (x : List Char) (counts : ColorCounts) (num : Nat) : Option (ColorCounts × List Char) :=
    match x with
      | 'r' :: 'e' :: 'd' ::               xs => ( parseList xs { counts with red := num } )
      | 'b' :: 'l' :: 'u' :: 'e' ::        xs => ( parseList xs { counts with blue := num } )
      | 'g' :: 'r' :: 'e' :: 'e' :: 'n' :: xs => ( parseList xs { counts with green := num } )
      | _ => none

  def parseList (x : List Char) (counts : ColorCounts) : Option (ColorCounts × List Char) :=
    match x with
      | [] => some ( counts , [] )
      | ' ' :: ',' :: ' ' :: y => getNum y counts
      | ',' :: ' ' ::        y => getNum y counts
      | ' ' ::               y => getNum y counts
      |                      y => getNum y counts
end

def colorsToColorCounts (s : String) : Option ColorCounts :=
  parseList s.data { red := 0 , blue := 0 , green := 0 } |> Option.map Prod.fst

def secondPart (x : String) : Option (List ColorCounts) := x
  |> mySplit ';'
  |> List.map (myDropWhile ' ')
  |> List.map colorsToColorCounts
  |> List.mapM id

def dealWithOneGame (x : String) : Option (Nat × ColorCounts) := x
  |> mySplit ':'
  |> twoElementListToProd
  |> Option.map (λ x => ( getGameNumber x.fst , secondPart x.snd))
  |> Option.map (Prod.mapM id id)
  |> Option.join
  |> Option.map (λ x => (x.fst, List.foldl ColorCounts.max { red := 0 , blue := 0 , green := 0 } x.snd))

def redLimit := 12
def greenLimit := 13
def blueLimit := 14

def filterAndSum (x : List ( Nat × ColorCounts ) ) := x
  |> List.filter (λ x => LE.le x.snd.red redLimit && LE.le x.snd.green greenLimit && LE.le x.snd.blue blueLimit )
  |> List.foldl (λ x y => x + y.fst) 0
  |> Nat.repr

def day2_1 (input : List String) : Option String := input
  |> List.map dealWithOneGame
  |> List.mapM id
  |> Option.map filterAndSum

def productAndSum (x : List ( Nat × ColorCounts ) ) := x
  |> List.map (λ x => x.snd.red * x.snd.green * x.snd.blue )
  |> List.foldl Nat.add 0
  |> Nat.repr

def day2_2 (input : List String) : Option String := input
  |> List.map dealWithOneGame
  |> List.mapM id
  |> Option.map productAndSum
