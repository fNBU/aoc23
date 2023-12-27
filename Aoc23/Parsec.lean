import Lean.Data.Parsec

open Lean Lean.Parsec ParseResult

namespace Parsec
  def toNat (p : Parsec String) : Parsec Nat := do
    let s ← p
    match s.toNat? with
    | some n => pure n
    | none => fail s!"expected a natural number"

  def optionalPrefixDiscard (a : Parsec String ) (p : Parsec α) : Parsec α :=
    attempt ( a *> p )

  def natDigits : Parsec Nat := toNat (manyChars digit)

  def manySingleChar (c : Char) : Parsec String := manyChars (pchar c)

  def spaces : Parsec String := manySingleChar ' '

  def manyNats : Parsec $ List Nat := do
    let a ← many $ optionalPrefixDiscard spaces natDigits
    return a.toList
end Parsec
