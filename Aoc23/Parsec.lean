import «Aoc23».Parsec_wrap

open Lean Parsec' Parsec.ParseResult

instance [BEq α] : BEq $ Parsec.ParseResult α where
  beq x y :=
    match x , y with
    | (success it a) , (success it' a') => it == it' && a == a'
    | (error it err) , (error it' err') => it == it' && err == err'
    | _ , _ => false

namespace Parsec'

  def toNat (p : Parsec' String) : Parsec' Nat := do
    let s ← p
    match s.toNat? with
    | some n => pure n
    | none => fail s!"expected a natural number"

  def nat : Parsec' Nat := toNat (manyChars digit)

  def manySingleChar (c : Char) : Parsec' String := manyChars (pchar c)

  def spaces : Parsec' String := manySingleChar ' '

  def sepBy1 {α β : Type} (p : Parsec' α) (sep : Parsec' β) : Parsec' $ Array α :=
    do
      let x ← p
      let xs ← many (sep *> p)
      return #[ x ] ++ xs

  def sepBy {α β : Type} (p : Parsec' α) (sep : Parsec' β) : Parsec' $ Array α :=
    sepBy1 p sep <|> pure #[]

  def sepByL  (p : Parsec' α) (sep : Parsec' β) : Parsec' $ List α := do
    let a ← sepBy p sep
    return a.toList

  def manyNats : Parsec' $ List Nat := sepByL nat ws

  def manyCharsList (l : List Char) : Parsec' $ List Char := do
    let a ← many $ satisfy l.contains
    return a.toList

  -- -- new implementations here, although they aren't necessary
  -- -- why reimplement `Char.isWhitespace`?
  -- def whitespace : Parsec' Char := satisfy Char.isWhitespace
  -- def manyWhitespace : Parsec' String := manyChars whitespace
  -- def ws' : Parsec' Unit := ( manyWhitespace *> pure () ) <|> pure ()

end Parsec'
