import AoCLib.Direction.*
import AoCLib.RotationDirection.CW
import AoCLib.{AoCUtil, Grid, Position}

class Day06(day: Int, name: String) extends AoCLib.Solution(day, name):
  override def solve(test: Boolean, index: Int): Unit =
    val input = AoCUtil.readInput(AoCUtil.fileName(day, test))

    solvePartOne(input)
  //solvePartTwo(input)

  def solvePartOne(input: List[String]): Unit =
    val map = Grid.load(input)
    val startCoord = map.coords(withValue = Some("^")).head
    var pos = Position(coord = startCoord, direction = N)
    var ext = map.extent.get
    while (ext.contains(pos.coord)) do
      map.set(pos.coord, "X")
      val ahead = map.getString(pos.coord.offset(pos.direction))
      if ahead == "#" then
        pos = pos.turn(CW)
      else
        pos = pos.moveForward()

    val visitedCount = map.coords(withValue = Some("X")).size

    println(s"Part One: the number of spots visited is $visitedCount")

  def solvePartTwo(input: String): Unit =

    println(s"Part Two: ")
