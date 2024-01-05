import Lean.Data.HashMap

open Lean HashMap

namespace HashMap

  instance [BEq α] [Hashable α] [Repr α] [Repr β] : Repr (HashMap α β) where
    reprPrec m _ := repr m.toList

  def mapToHashMap ( f : α -> δ ) ( g : β -> γ ) [ BEq α ] [ Hashable α ] [ BEq δ ] [ Hashable δ ] ( m : HashMap α β) : HashMap δ γ :=
    m.fold (fun x a b => x.insert (f a) (g b)) empty

  def indices [BEq α] [Hashable α] (m : HashMap α β) : List α := m.toList.map Prod.fst

  def mapAt [BEq α] [Hashable α] (m : HashMap α β) (i : α) (f : β → β) : HashMap α β :=
    match m.find? i with
    | none => m
    | some v => m.insert i (f v)

  def mapAtList [BEq α] [Hashable α] (m : HashMap α β) (l : List α) (f : β → β) : HashMap α β :=
    l.foldl (mapAt (f := f)) m

end HashMap
