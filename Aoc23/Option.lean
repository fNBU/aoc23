def Option.join {α : Type} (x : Option (Option α)) : Option α := Option.bind x id
