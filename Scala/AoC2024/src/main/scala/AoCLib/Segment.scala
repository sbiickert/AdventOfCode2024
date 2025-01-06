package AoCLib

import AoCLib.Direction._

case class Segment(from: Coord, to:Coord):
  override def toString: String = s"$from -> $to"

  def length: Double =
    from.manhattanDistanceTo(to)

  def isHorizontal:Boolean =
    from.y == to.y && from.x != to.x

  def isVertical:Boolean =
    from.x == to.x && from.y != to.y

  def direction: Option[Direction] =
    if length == 0 then return None

    if isHorizontal then
      return Some(if from.x < to.x then Direction.E else W)
    if isVertical then
      return Some(if from.y < to.y then Direction.S else N)

    if from.x < to.x then
      return Some(if from.y < to.y then Direction.SE else NE)
    return Some(if from.y < to.y then Direction.SW else NW)

