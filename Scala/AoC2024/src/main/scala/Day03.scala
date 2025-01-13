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
  val sum = input.split("""do\(\)""")                    // Split on do()
    .map(doBlock => doBlock.split("""don't\(\)""").head) // Only analyze up to don't()
    .map(sumIn)                                          // Sum for each do() block
    .sum                                                 // Grand total
  println(s"Part Two: the sum of mul statements is $sum")

def sumIn(str: String): Int =
  val re = """mul\((\d+),(\d+)\)""".r
  re.findAllIn(str).matchData
    .map(m => m.group(1).toInt * m.group(2).toInt)
    .sum



