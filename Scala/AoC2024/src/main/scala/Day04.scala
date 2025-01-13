import AoCLib.{AdjacencyRule, AoCUtil, Direction, Grid}

class Day04(day: Int, name: String) extends AoCLib.Solution(day, name):
  override def solve(test: Boolean, index: Int): Unit =
    val input = AoCUtil.readInput(AoCUtil.fileName(day, test))
    val grid = Grid.load(input)

    solvePartOne(grid)
  //solvePartTwo(grid)

  def solvePartOne(grid: Grid): Unit =
    val xCoords = grid.coords(withValue = Some("X"))
    var count = 0
    for xc <- xCoords do
      for dir <- AdjacencyRule.Queen.directions do
        var word = "X"
        for size <- 1 to 3 do
          word += grid.getString(xc.offset(dir, size))
        if word == "XMAS" then count += 1

    println(s"Part One: the total number of XMAS is $count")

  def solvePartTwo(grid: Grid): Unit =

    println(s"Part Two: ")
