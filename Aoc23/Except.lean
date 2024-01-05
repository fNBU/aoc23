namespace Except

  def join (x : Except δ (Except δ α)) := Except.bind x id

  def beq [BEq ε] [BEq α] (x y : Except ε α) : Bool :=
    match x, y with
    | Except.ok x, Except.ok y => x == y
    | Except.error x, Except.error y => x == y
    | _, _ => false

end Except

instance [BEq ε] [BEq α] : BEq $ Except ε α where
  beq := Except.beq

instance [Hashable ε] [Hashable α] : Hashable $ Except ε α where
  hash x := match x with
    | Except.ok x => mixHash (hash "ok") (hash x)
    | Except.error x => mixHash (hash "error") (hash x)

instance [Repr ε] [Repr α] : Repr $ Except ε α where
  reprPrec x _ := match x with
    | Except.ok x => "(Except.ok " ++ repr x ++ ")"
    | Except.error x => "(Except.error " ++ repr x ++ ")"

instance [ HAdd α β γ ] : HAdd ( Except δ α ) β ( Except δ γ ) where
  hAdd x y := Except.map (· + y) x

instance [ HAdd α β γ ] : HAdd α ( Except δ β ) ( Except δ γ ) where
  hAdd x y := Except.map (x + ·) y

instance [ HAdd α β γ ] : HAdd ( Except δ α ) ( Except δ β ) ( Except δ γ ) where
  hAdd x y := pure (·+·) <*> x <*> y
