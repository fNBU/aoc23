def getFirstDigit (s : List Char) : Option Char :=
  match s with
  | [] => none
  | '0' :: _ => some '0'
  | '1' :: _ => some '1'
  | '2' :: _ => some '2'
  | '3' :: _ => some '3'
  | '4' :: _ => some '4'
  | '5' :: _ => some '5'
  | '6' :: _ => some '6'
  | '7' :: _ => some '7'
  | '8' :: _ => some '8'
  | '9' :: _ => some '9'
  | _ :: as => getFirstDigit as

def getFirstAndLastDigit (s: String) : Option (List Char) :=
  let d := s.data
  [ getFirstDigit d , getFirstDigit ( List.reverse d ) ].mapM id

def joinChars (p : Char × Char) : String :=
  [ p.fst , p.snd ].asString

def Option.join {α : Type} (x : Option (Option α)) : Option α := Option.bind x id

def digitListToInt (x : List Char) : Option Int := x.asString.toInt?

def processOneLine (f : α → Option (List Char)) (x : α) : Option Int := x
  |> f
  |> Option.map digitListToInt
  |> Option.join

def day1_1 (input : List String) : Option String := input
  |> List.map (processOneLine getFirstAndLastDigit)
  |> List.mapM id
  |> Option.map (List.foldl Int.add 0)
  |> Option.map (Int.repr)

-- 2

def getFirstDigitForward (s : List Char) : Option Char :=
  match s with
  | [] => none
  | '0' :: _ => some '0'
  | '1' :: _ => some '1'
  | '2' :: _ => some '2'
  | '3' :: _ => some '3'
  | '4' :: _ => some '4'
  | '5' :: _ => some '5'
  | '6' :: _ => some '6'
  | '7' :: _ => some '7'
  | '8' :: _ => some '8'
  | '9' :: _ => some '9'
  | 'o' :: 'n' :: 'e' :: _ => some '1'
  | 't' :: 'w' :: 'o' :: _ => some '2'
  | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ => some '3'
  | 'f' :: 'o' :: 'u' :: 'r' :: _ => some '4'
  | 'f' :: 'i' :: 'v' :: 'e' :: _ => some '5'
  | 's' :: 'i' :: 'x' :: _ => some '6'
  | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ => some '7'
  | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ => some '8'
  | 'n' :: 'i' :: 'n' :: 'e' :: _ => some '9'
  | _ :: as => getFirstDigitForward as

def getFirstDigitBackward (s : List Char) : Option Char :=
  match s with
  | [] => none
  | '0' :: _ => some '0'
  | '1' :: _ => some '1'
  | '2' :: _ => some '2'
  | '3' :: _ => some '3'
  | '4' :: _ => some '4'
  | '5' :: _ => some '5'
  | '6' :: _ => some '6'
  | '7' :: _ => some '7'
  | '8' :: _ => some '8'
  | '9' :: _ => some '9'
  | 'e' :: 'n' :: 'o' :: _ => some '1'
  | 'o' :: 'w' :: 't' :: _ => some '2'
  | 'e' :: 'e' :: 'r' :: 'h' :: 't' :: _ => some '3'
  | 'r' :: 'u' :: 'o' :: 'f' :: _ => some '4'
  | 'e' :: 'v' :: 'i' :: 'f' :: _ => some '5'
  | 'x' :: 'i' :: 's' :: _ => some '6'
  | 'n' :: 'e' :: 'v' :: 'e' :: 's' :: _ => some '7'
  | 't' :: 'h' :: 'g' :: 'i' :: 'e' :: _ => some '8'
  | 'e' :: 'n' :: 'i' :: 'n' :: _ => some '9'
  | _ :: as => getFirstDigitBackward as

def getFirstAndLastDigit2 (s: String) : Option (List Char) :=
  let d := s.data
  [ getFirstDigitForward d , getFirstDigitBackward ( List.reverse d ) ].mapM id

def day1_2 (input : List String) := input
  |> List.map (processOneLine getFirstAndLastDigit2)
  |> List.mapM id
  |> Option.map (List.foldl Int.add 0)
  |> Option.map (Int.repr)
