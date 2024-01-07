open List

namespace List

  def filterOption ( l : List (Option α) ) : List α :=
    let f ( accum : List α ) ( x : Option α ) :=
      match x with
      | some x => x :: accum
      | none => accum
    foldl f [] l

  def filterExcept ( l : List (Except β α) ) : List α :=
    l |> map ( fun x => x.toOption ) |> filterOption

  instance : Seq List where
    seq f x := List.bind f fun y => Functor.map y ( x () )

  def flatten (x : List (List α)) := List.bind x id

  def init (n : Nat) (f : Nat → α) : List α :=
    let rec helper n accum :=
      match n with
      | 0 => accum
      | n + 1 => helper n ( ( f n ) ::accum )
    helper n []

  def distinct [ BEq α ] ( l : List α ) : List α :=
    let f ( accum : List α ) ( x : α ) :=
      if contains accum x then
        accum
      else
        x :: accum
    foldl f [] l

  def intersection [ BEq α ] ( x y : List α ) : List α :=
    let rec helper x accum :=
      match x with
      | [] => accum
      | z :: zs =>
        if contains y z then
          helper zs ( z :: accum )
        else
          helper zs accum
    helper x []

  def minus [ BEq α ] ( x y : List α ) : List α :=
    let rec helper (x : List α) (y' : Array $ Option α) (accum : List α) :=
      match x with
      | [] => accum
      | z :: zs =>
        match Array.findIdx? y' ( BEq.beq ( some z ) ) with
        | some i => helper zs (y'.setD i none ) accum
        | none => helper zs y' ( z :: accum )
    let y' := y.toArray.map (λ a => some a)
    helper x y' []

  def sort (l : List α) (lt : α → α → Bool) := Array.toList ( Array.qsort l.toArray lt )

  /- The order of arguments for `sort` above is convenient if you are usually using `sort` method-style (i.e. `l.sort (.>.)`). The definition below is more useful for `$` and `|>` style (i.e. `sort (.>.) $ l` or `l |> sort (.>.)`). -/

  def sortR (lt : α → α → Bool) (l : List α) := sort l lt

  /--
  Sorts based on hash. Doesn't provide a human-usable sort, but canonicalizes an iterable.
  -/
  def sortD [Hashable α] (l : List α) := l.sort (fun x y => hash x < hash y)

  def ord [Ord α] (l m : List α) : Ordering :=
    match l, m with
    | [] , [] => Ordering.eq
    | [] , _ => Ordering.lt
    | _ , [] => Ordering.gt
    | (a::as), (b::bs) =>
      match compare a b with
      | Ordering.eq => ord as bs
      | o => o

  instance [Ord α] : Ord (List α) where
    compare a b := ord a b

  def part [BEq α] (l : List α) : List $ α × Nat :=
    let distinct := l.distinct
    distinct.foldl (
      λ acc x =>
      ( x, ( l.filter ( λ y => y == x ) ).length ) :: acc
    ) []

  -- Make the interface for List similar to HashMap, so we can easily change implementations.
  def empty : List α := []
  def fold (f : α → β → α) (init : α) (l : List β) : α := foldl f init l
  def toList ( l : List α ) := l

end List
