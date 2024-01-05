import Lean.Data.Parsec
import «Aoc23».Util
import «Aoc23».Except


def getFirstDigit (s : List Char) : Ex Char :=
  match s with
  | [] => Except.error "getFirstDigit: Got empty list"
  | '0' :: _ => Except.ok '0'
  | '1' :: _ => Except.ok '1'
  | '2' :: _ => Except.ok '2'
  | '3' :: _ => Except.ok '3'
  | '4' :: _ => Except.ok '4'
  | '5' :: _ => Except.ok '5'
  | '6' :: _ => Except.ok '6'
  | '7' :: _ => Except.ok '7'
  | '8' :: _ => Except.ok '8'
  | '9' :: _ => Except.ok '9'
  | _ :: as => getFirstDigit as

def getFirstAndLastDigit (s: String) : Ex (List Char) :=
  let d := s.data
  [ getFirstDigit d , getFirstDigit ( List.reverse d ) ].mapM id

def joinChars (p : Char × Char) : String :=
  [ p.fst , p.snd ].asString

def digitListToInt (x : List Char) : Ex Int :=
  match x.asString.toInt? with
  | some i => Except.ok i
  | none => Except.error "digitListToInt: Couldn't convert to int"

def processOneLine (f : α → Ex (List Char)) (x : α) : Ex Int :=
  x |> f |> Except.map digitListToInt |> Except.join

def day1_1 (input : List String) : Ex String :=
  processOneLine getFirstAndLastDigit <$> input
  |> List.foldl (· + ·) ( pure 0 )
  |> Except.map Int.repr

def getFirstDigitForward (s : List Char) : Ex Char :=
  match s with
  | [] => Except.error "getFirstDigitForward: Got empty list"
  | '0' :: _ => Except.ok '0'
  | '1' :: _ => Except.ok '1'
  | '2' :: _ => Except.ok '2'
  | '3' :: _ => Except.ok '3'
  | '4' :: _ => Except.ok '4'
  | '5' :: _ => Except.ok '5'
  | '6' :: _ => Except.ok '6'
  | '7' :: _ => Except.ok '7'
  | '8' :: _ => Except.ok '8'
  | '9' :: _ => Except.ok '9'
  | 'o' :: 'n' :: 'e' :: _ => Except.ok '1'
  | 't' :: 'w' :: 'o' :: _ => Except.ok '2'
  | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ => Except.ok '3'
  | 'f' :: 'o' :: 'u' :: 'r' :: _ => Except.ok '4'
  | 'f' :: 'i' :: 'v' :: 'e' :: _ => Except.ok '5'
  | 's' :: 'i' :: 'x' :: _ => Except.ok '6'
  | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ => Except.ok '7'
  | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ => Except.ok '8'
  | 'n' :: 'i' :: 'n' :: 'e' :: _ => Except.ok '9'
  | _ :: as => getFirstDigitForward as

def getFirstDigitBackward (s : List Char) : Ex Char :=
  match s with
  | [] => Except.error "getFirstDigitBackward: Got empty list"
  | '0' :: _ => Except.ok '0'
  | '1' :: _ => Except.ok '1'
  | '2' :: _ => Except.ok '2'
  | '3' :: _ => Except.ok '3'
  | '4' :: _ => Except.ok '4'
  | '5' :: _ => Except.ok '5'
  | '6' :: _ => Except.ok '6'
  | '7' :: _ => Except.ok '7'
  | '8' :: _ => Except.ok '8'
  | '9' :: _ => Except.ok '9'
  | 'e' :: 'n' :: 'o' :: _ => Except.ok '1'
  | 'o' :: 'w' :: 't' :: _ => Except.ok '2'
  | 'e' :: 'e' :: 'r' :: 'h' :: 't' :: _ => Except.ok '3'
  | 'r' :: 'u' :: 'o' :: 'f' :: _ => Except.ok '4'
  | 'e' :: 'v' :: 'i' :: 'f' :: _ => Except.ok '5'
  | 'x' :: 'i' :: 's' :: _ => Except.ok '6'
  | 'n' :: 'e' :: 'v' :: 'e' :: 's' :: _ => Except.ok '7'
  | 't' :: 'h' :: 'g' :: 'i' :: 'e' :: _ => Except.ok '8'
  | 'e' :: 'n' :: 'i' :: 'n' :: _ => Except.ok '9'
  | _ :: as => getFirstDigitBackward as

def getFirstAndLastDigit2 (s: String) : Ex (List Char) :=
  let d := s.data
  [ d |> getFirstDigitForward , d |> List.reverse |> getFirstDigitBackward ].mapM id

def day1_2 (input : List String) :=
  processOneLine getFirstAndLastDigit2 <$> input
  |> List.foldl (· + ·) ( pure 0 )
  |> Except.map Int.repr
