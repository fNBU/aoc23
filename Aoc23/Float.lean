import «Aoc23».Int

namespace Float

  def toIntCeil (f : Float) : Int :=
    if f >= 0.0 then
      f.ceil.toUInt64.toNat
    else
      -(f.neg.floor.toUInt64.toNat)

  def toIntFloor (f : Float) : Int :=
    if f >= 0.0 then
      f.floor.toUInt64.toNat
    else
      -(f.neg.ceil.toUInt64.toNat)

  def toIntTowardZero (f : Float) : Int :=
    if f >= 0.0 then
      f.toIntFloor
    else
      f.toIntCeil

  def intAndFrac (f : Float) : Int × Float :=
    ( f.toIntTowardZero , f - f.toIntTowardZero.toFloat )

end Float
