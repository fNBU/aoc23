open Lean List

namespace List
  def filterOption ( l : List (Option α) ) : List α :=
    let f ( accum : List α ) ( x : Option α ) :=
      match x with
      | some x => x :: accum
      | none => accum
    foldl f [] l

  instance : Seq List where
    seq f x := List.bind f fun y => Functor.map y ( x () )

  def flatten (x : List (List α)) := List.bind x id

  def init (n : Nat) (f : Nat → α) : List α :=
    let rec helper n accum :=
      match n with
      | 0 => accum
      | n + 1 => helper n ( ( f n ) ::accum )
    helper n []

  def distinct ( l : List α ) [ BEq α ] : List α :=
    let f ( accum : List α ) ( x : α ) :=
      if contains accum x then
        accum
      else
        x :: accum
    foldl f [] l

  def intersection ( x y : List α ) [ BEq α ] : List α :=
    let rec helper x accum :=
      match x with
      | [] => accum
      | z :: zs =>
        if contains y z then
          helper zs ( z :: accum )
        else
          helper zs accum
    helper x []

  def sort (l : List α) (lt : α → α → Bool) := Array.toList ( Array.qsort l.toArray lt )

  /-- The order of arguments for `sort` above is convenient if you are usually using `sort` method-style (i.e. `l.sort (.>.)`). The definition below is more useful for `$` and `|>` style (i.e. `sort (.>.) $ l` or `l |> sort (.>.)`). -/

  def sortR (lt : α → α → Bool) (l : List α) := sort l lt

  -- Make the interface for List similar to HashMap, so we can easily change implementations.
  def empty : List α := []
  def fold (f : α → β → α) (init : α) (l : List β) : α := foldl f init l
  def toList ( l : List α ) := l
end List
