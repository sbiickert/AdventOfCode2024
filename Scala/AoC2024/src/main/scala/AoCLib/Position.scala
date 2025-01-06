package AoCLib

import AoCLib.Direction._

case class Position(coord: Coord = Coord.origin, direction: Direction = N):
  
  override def toString: String = s"{$coord $direction}"

  def turn(dir: RotationDirection): Position =
    val ordered = AdjacencyRule.Rook.directions
    val index = ordered.indexOf(direction)
    val turnedIndex = (index + dir.step) % 4
    Position(coord, ordered(turnedIndex))

  def turn(dir: String): Position =
    val rotationDirection = RotationDirection.fromString(dir)
    rotationDirection match
      case Some(rd) => turn(rd)
      case None     => this // no turn

  def moveForward(distance:Int = 1): Position =
    val offset = Coord.offset(direction)
    val move = Coord(offset.x * distance, offset.y * distance)
    Position(coord + move, direction)

object Position:
  
  def withString(coord: Coord, directionStr: String): Position =
    Position(coord, Direction.fromString(directionStr))