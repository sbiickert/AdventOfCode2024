import AoCLib.AoCUtil

class Day03(day: Int, name: String) extends AoCLib.Solution(day, name):
  override def solve(test: Boolean, index: Int): Unit =
    val input = AoCUtil.readGroupedInput(AoCUtil.fileName(day, test))(index)
    val allInput = input.mkString // Challenge input is across multiple lines

    solvePartOne(allInput)
    solvePartTwo(allInput)

def solvePartOne(input: String): Unit =
  val sum = sumIn(input)
  println(s"Part One: the sum of mul statements is $sum")

def solvePartTwo(input: String): Unit =
  var sum = 0
  val doBlocks = input.split("""do\(\)""")
  for doBlock <- doBlocks do
    val untilDont = doBlock.split("""don't\(\)""").head
    sum += sumIn(untilDont)

  println(s"Part Two: the sum of mul statements is $sum")

def sumIn(str: String): Int =
  val re = """mul\((\d+),(\d+)\)""".r
  re.findAllIn(str).matchData
    .map(m => m.group(1).toInt * m.group(2).toInt)
    .sum



