import AoCLib.AoCUtil

class Day03(day: Int, name: String) extends AoCLib.Solution(day, name):
  override def solve(test: Boolean, index: Int): Unit =
    val input = AoCUtil.readGroupedInput(AoCUtil.fileName(day, test))(index)
    val allInput = input.mkString // Challenge input is across multiple lines

    solvePartOne(allInput)
//    solvePartTwo(allInput)

def solvePartOne(input: String): Unit =
  val re = """mul\((\d+),(\d+)\)""".r
  val allMatches = re.findAllIn(input)
  val sum = allMatches.matchData
    .map(m => m.group(1).toInt * m.group(2).toInt)
    .sum

  println(s"Part One: the sum of mul statements is $sum")

def solvePartTwo(input: String): Unit =
  ???


