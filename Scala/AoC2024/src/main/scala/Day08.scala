import AoCLib.AdjacencyRule.Queen
import AoCLib.{AoCUtil, Coord, Extent, Grid}

class Day08(day: Int, name: String) extends AoCLib.Solution(day, name):
  override def solve(test: Boolean, index: Int): Unit =
    val input = AoCUtil.readGroupedInput(AoCUtil.fileName(day, test))(0)
    val grid = Grid.load(data = input, rule = Queen)
    val coordsByValue = grid.histogram(includeUnset = false)
      .map((k:String, _:Int) => (k, grid.coords(withValue = Some(k))))

    //grid.print()
    solvePartOne(coordsByValue, grid.extent.get)
    solvePartTwo(coordsByValue, grid.extent.get)

  def solvePartOne(nodes: Map[String, Iterable[Coord]], extent: Extent): Unit =
    val antiNodes = Grid()
    for (_, coords) <- nodes do
      for combo <- coords.toList.combinations(2) do
        val antiNode0 = combo(0) + combo(1).delta(combo(0))
        val antiNode1 = combo(1) + combo(0).delta(combo(1))
        if extent.contains(antiNode0) then antiNodes.set(antiNode0, "#")
        if extent.contains(antiNode1) then antiNodes.set(antiNode1, "#")

    //antiNodes.print()
    val antiNodeCount = antiNodes.coords(withValue = Some("#")).size
    println(s"Part One: the number of antinodes is $antiNodeCount")


def solvePartTwo(nodes: Map[String, Iterable[Coord]], extent: Extent): Unit =
  val antiNodes = Grid()
  for (_, coords) <- nodes do
    for combo <- coords.toList.combinations(2) do
      antiNodes.set(combo(0), "#")
      antiNodes.set(combo(1), "#")
      val delta0 = combo(1).delta(combo(0))
      val delta1 = combo(0).delta(combo(1))
      
      var antiNode = combo(0) + delta0
      while extent.contains(antiNode) do
        antiNodes.set(antiNode, "#")
        antiNode = antiNode + delta0

      antiNode = combo(1) + delta1
      while extent.contains(antiNode) do
        antiNodes.set(antiNode, "#")
        antiNode = antiNode + delta1

  //antiNodes.print()
  val antiNodeCount = antiNodes.coords(withValue = Some("#")).size
  println(s"Part Two: the number of antinodes is $antiNodeCount")
