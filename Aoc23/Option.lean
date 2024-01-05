namespace Option

  def toExcept (x : Option α) (err : δ) : Except δ α :=
    match x with
    | none => Except.error err
    | some x => Except.ok x

  -- #eval Option.toExcept (some 3) "error"  -- ok 3
  -- #eval Option.toExcept ( none : Option Nat ) "this is an error" -- Except.error "this is an error"

  /--
  If `δ` is `Inhabited`, uses `default` as the error message.
  -/
  def toExceptD [Inhabited δ] (x : Option α) : Except δ α :=
    match x with
    | none => Except.error default
    | some x => Except.ok x

  -- #eval ( ( Option.toExceptD (some 3) ) : Except Nat Nat )-- ok 3
  -- #eval ( ( Option.toExceptD ( none : Option Nat ) ) : Except Nat Nat ) -- Except.error 0

end Option


/-
-- One may be tempted to write

instance [ Functor F ] [ HAdd α β γ ] : HAdd ( F α ) β ( F γ ) where
  hAdd x y := Functor.map (· + y) x

-- In order to see why this is not a good idea, consider the case when `F` is `List`.

#eval [1 , 2 ] + [3 , 4]
-/

instance [ HAdd α β γ ] : HAdd ( Option α ) β ( Option γ ) where
  hAdd x y := Option.map (· + y) x

instance [ HAdd α β γ ] : HAdd α ( Option β ) ( Option γ ) where
  hAdd x y := Option.map (x + ·) y

instance [ HAdd α β γ ] : HAdd ( Option α ) ( Option β ) ( Option γ ) where
  hAdd x y := pure (·+·) <*> x <*> y

-- #eval (some 3) + (some 5)  -- some 8
