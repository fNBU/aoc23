namespace Prod

  def mapM [Monad m] (f : α → m β) (g : γ → m δ) (as : α × γ) : m (β × δ) := do
    let (a,b) := as
    pure ( (← f a) , (← g b) )

end Prod
