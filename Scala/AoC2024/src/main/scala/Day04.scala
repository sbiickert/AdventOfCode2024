import AoCLib.AoCUtil
import AoCLib.Grid

class Day04(day: Int, name: String) extends AoCLib.Solution(day, name):
  override def solve(test: Boolean, index: Int): Unit =
    val input = AoCUtil.readInput(AoCUtil.fileName(day, test))
    val grid = Grid.load(input)
    grid.print()