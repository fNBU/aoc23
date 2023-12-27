def Option.flatten (x : Option (Option α)) := Option.bind x id

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
