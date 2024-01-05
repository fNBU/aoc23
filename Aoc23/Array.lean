import Lean.Data.HashSet

open Lean

namespace Array

  /--
  Beware that `HashSet` doesn't track multiplicity.
  -/
  def toHashSet [BEq α] [Hashable α] (a : Array α) : HashSet α  :=
    a.foldl (λ s a => s.insert a) {}

end Array
