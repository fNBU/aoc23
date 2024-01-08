import Lean.Data.Parsec

open Lean String Parsec.ParseResult

def Lean.Parsec.ParseResult.map (x : Parsec.ParseResult α) (f : α -> β) : Parsec.ParseResult β :=
    match x with
    | success it a => success it (f a)
    | error it err => error it err

/--
A parser on `String`s, returning type `α` or a failure.

NB: This library is a safe string parser that mostly wraps `Lean.Parsec` by Dany Fabian. There are some important differences:

  1. Our implementation of `many` (and so also `manyChars`, etc) has proof of termination. This implies that `many p` returns an error if `p` does not return a shorter iterator on each recursive call.

  2. We omit `skipWs : Iterator -> Iterator`. You can use `ws : Parsec' Unit` (which also exists in `Lean.Parsec`).

  3. To preserve safety, we omit `peek!`. Use `peek?` instead.
-/
def Parsec' α := Parsec α

namespace Parsec'

  -- wrapping Lean.Parsec
  instance : Inhabited (Parsec' α) := ⟨λ it => error it default⟩
  private def pure (a : α) : Parsec' α := Lean.Parsec.pure a
  def bind {α β : Type} (f : Parsec' α) (g : α → Parsec' β) : Parsec' β := Lean.Parsec.bind f g
  instance : Monad Parsec' := { pure := pure, bind := bind }
  def orElse (p : Parsec' α) (q : Unit → Parsec' α) : Parsec' α := Lean.Parsec.orElse p q
  instance : OrElse $ Parsec' α := ⟨orElse⟩

  -- new stuff
  /--
  The internals of all the `many...` functions. Guaranteed to terminate. `many p` returns (with `success`) if `p` returns `success` without returning a shorter iterator.

  NB: This behavior differs from that in `Lean.Parsec`.
  -/
  private def manyHelper (p : Parsec' α ) (acc : Array α) (remaining : Nat): Parsec' $ Array α :=
    λ it =>
      if remaining == 0 then
        success it acc
      else
        let x := p it
        match x with
        | error _ _ => success it acc
        | success pos a =>
          let remaining' := pos.s.length - pos.i.byteIdx
          if remaining' < remaining then
            manyHelper p (acc.push a) remaining' pos
          else
            success it acc

  /--
  A wrapper for `manyHelper` that has the same interface as `Lean.Parsec.manyCore`.
  -/
  private def manyCore (p : Parsec' α) (acc : Array α) : Parsec' $ Array α :=
    λ it =>
      manyHelper p acc ( it.s.length - it.i.byteIdx ) it

  -- need to repeat these definitions so that they pick up the new `manyCore`
  def many (p : Parsec' α) : Parsec' $ Array α := manyCore p #[]
  def many1 (p : Parsec' α) : Parsec' $ Array α := do manyCore p #[←p]

  -- why implement `manyCore` twice?
  def manyCharsCore (p : Parsec' Char) (acc : String) : Parsec' String :=
    String.mk <$> Array.toList <$> manyCore p acc.data.toArray

  def manyChars (p : Parsec' Char) : Parsec' String := manyCharsCore p ""
  def many1Chars (p : Parsec' Char) : Parsec' String := do manyCharsCore p (←p).toString

  -- wrapping Lean.Parsec
  def satisfy (p : Char → Bool) : Parsec' Char := Lean.Parsec.satisfy p


  -- wrapping Lean.Parsec
  def ws : Parsec' Unit := Lean.Parsec.ws
  def fail (x : String) : Parsec' α := Lean.Parsec.fail x
  instance : Alternative Parsec' := { failure := fail "", orElse := orElse }
  def attempt (p : Parsec' α) : Parsec' α := Lean.Parsec.attempt p
  def run (p : Parsec' α) (input : String) : Except String α := Lean.Parsec.run p input
  def eof : Parsec' Unit := Lean.Parsec.eof
  def pstring (s : String) : Parsec' String := Lean.Parsec.pstring s
  def skipString (s : String) : Parsec' Unit := Lean.Parsec.skipString s
  def anyChar : Parsec' Char := Lean.Parsec.anyChar
  def pchar (c : Char) : Parsec' Char := Lean.Parsec.pchar c
  def skipChar (c : Char) : Parsec' Unit := Lean.Parsec.skipChar c
  def digit : Parsec' Char := Lean.Parsec.digit
  def hexDigit : Parsec' Char := Lean.Parsec.hexDigit
  def asciiLetter : Parsec' Char := Lean.Parsec.asciiLetter
  def notFollowedBy (p : Parsec' α) : Parsec' Unit := Lean.Parsec.notFollowedBy p
  def peek? : Parsec' (Option Char) := Lean.Parsec.peek?
  def skip : Parsec Unit := Lean.Parsec.skip

end Parsec'
