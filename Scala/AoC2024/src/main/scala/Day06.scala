import AoCLib.Direction.N
import AoCLib.RotationDirection.CW
import AoCLib.{AoCUtil, Grid, Position, Coord}
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._

class Day06(day: Int, name: String) extends AoCLib.Solution(day, name):
  override def solve(test: Boolean, index: Int): Unit =
    val input = AoCUtil.readInput(AoCUtil.fileName(day, test))

    val visitedCoords = solvePartOne(input)
    solvePartTwo(input, visitedCoords)

  def solvePartOne(input: List[String]): List[Coord] =
    val grid = Grid.load(input)
    val startCoord = grid.coords(withValue = Some("^")).head
    var pos = Position(coord = startCoord, direction = N)
    val ext = grid.extent.get
    while (ext.contains(pos.coord)) do
      grid.set(pos.coord, "X")
      pos = makeMove(pos, grid)

    val visited = grid.coords(withValue = Some("X"))
    val visitedCount = visited.size
    println(s"Part One: the number of spots visited is $visitedCount")
    visited.filterNot(_ == startCoord).toList

  def solvePartTwo(input: List[String], visited: List[Coord]): Unit =
    val grid = Grid.load(input)
    val startCoord = grid.coords(withValue = Some("^")).head
    val pos = Position(coord = startCoord, direction = N)

    // Using Parallel Collection for speed
    // https://docs.scala-lang.org/overviews/parallel-collections/overview.html
    val loopOrNot = visited.par.map(c => doesItLoop(pos, grid, barrier = c))
    val loopingCount = loopOrNot.count(_ == true)

    println(s"Part Two: the number of spots that cause a loop is $loopingCount")

  private def makeMove(pos: Position, map: Grid, barrier: Option[Coord] = None): Position =
    val aheadCoord = pos.coord.offset(pos.direction)
    var aheadValue = map.getString(aheadCoord)
    if barrier.nonEmpty && barrier.get == aheadCoord then
      aheadValue = "#"
    if aheadValue == "#" then
      pos.turn(CW)
    else
      pos.moveForward()

  private def doesItLoop(pos: Position, grid: Grid, barrier: Coord): Boolean =
    var position = pos
    val history = mutable.HashSet.empty[Position]
    var loops = true
    while (loops && !history.contains(position)) do
      history.addOne(position)
      position = makeMove(position, grid, Some(barrier))
      if !grid.extent.get.contains(position.coord) then
        loops = false
    loops