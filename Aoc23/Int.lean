namespace Int

  def toFloat (i : Int) : Float :=
    if i >= 0 then
      i.toNat.toFloat
    else
      (-i).toNat.toFloat.neg

end Int
