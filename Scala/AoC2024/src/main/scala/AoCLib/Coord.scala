package AoCLib

import AoCLib.AdjacencyRule._
import Direction._

case class Coord(x: Int, y: Int):
  def col:Int = x
  def row:Int = y

  override def toString: String = s"[$x,$y]"

  def +(other: Coord) =
    Coord(x + other.x, y + other.y)

  def delta(other: Coord): Coord =
    Coord(other.x - x, other.y - y)

  def offset(dir: Direction, size: Int = 1): Coord =
    if size == 0 then
      return this

    val off = Coord.offset(dir)
    if size == 1 then
      return this + off

    val bigOff = Coord(off.x * size, off.y * size)
    this + bigOff

  def isAdjacentTo(other: Coord, rule: AdjacencyRule = Rook):Boolean =
    rule match
      case Rook => manhattanDistanceTo(other) == 1
      case Bishop =>
        Math.abs(x - other.x) == 1 && Math.abs(y - other.y) == 1
      case Queen =>
        manhattanDistanceTo(other) == 1 ||
          (Math.abs(x - other.x) == 1 && Math.abs(y - other.y) == 1)

  def manhattanDistanceTo(other: Coord): Int =
    Math.abs(x - other.x) + Math.abs(y - other.y)

  def distanceTo(other: Coord): Double =
    val del = delta(other)
    math.sqrt(math.pow(del.x, 2) + math.pow(del.y ,2))

  def getAdjacentCoords(rule: AdjacencyRule): List[Coord] =
    rule.directions.map(dir => this.offset(dir))



object Coord:
  def origin: Coord =
    Coord(0,0)

  def fromString(str:String): Option[Coord] =
    val pattern = """\[(-?\d+),(-?\d+)\]""".r
    str match
      case pattern(xStr, yStr) => Some(Coord(xStr.toInt, yStr.toInt))
      case _ => None

  def offset(str: String): Coord =
    val dir = Direction.fromString(str)
    offset(dir)

  def offset(dir: Direction): Coord =
    dir match
      case N => Coord(0, -1)
      case NE => Coord(1, -1)
      case E => Coord(1, 0)
      case SE => Coord(1, 1)
      case S => Coord(0, 1)
      case SW => Coord(-1, 1)
      case W => Coord(-1, 0)
      case NW => Coord(-1, -1)
