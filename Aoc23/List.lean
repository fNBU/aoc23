open Lean

def List.filterOption [Inhabited α] ( l : List (Option α) ) : List α :=
  l |> List.filter Option.toBool |> List.map Option.get!

instance : Seq List where
  seq f x := List.bind f fun y => Functor.map y ( x () )

def List.flatten (x : List (List α)) := List.bind x id

def List.init (n : Nat) (f : Nat → α) : List α :=
  let rec helper n accum :=
    match n with
    | 0 => accum
    | n + 1 => helper n ( ( f n ) ::accum )
  helper n []

def List.distinct ( l : List α ) [ BEq α ] : List α :=
  let rec helper l accum :=
    match l with
    | [] => accum
    | x :: xs =>
      if List.contains accum x then
        helper xs accum
      else
        helper xs (x::accum)
  helper l []

def List.intersection ( x y : List α ) [ BEq α ] : List α :=
  let rec helper x accum :=
    match x with
    | [] => accum
    | z :: zs =>
      if List.contains y z then
        helper zs ( z :: accum )
      else
        helper zs accum
  helper x []

-- Make the interface for List similar to HashMap, so we can easily change implementations.
def List.empty : List α := []
def List.fold (f : α → β → α) (init : α) (l : List β) : α := List.foldl f init l
def List.toList ( l : List α ) := l
