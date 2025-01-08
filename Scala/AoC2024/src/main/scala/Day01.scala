import AoCLib.AoCUtil

import scala.collection.mutable.ArrayBuffer

class Day01(day: Int, name: String) extends AoCLib.Solution(day, name):
  override def solve(test: Boolean, index: Int): Unit =
    val input = AoCUtil.readInput(AoCUtil.fileName(day, test))
      .map(line =>
        val pattern = """(\d+)\s+(\d+)""".r
        line match
          case pattern(d1, d2) => List(d1.toInt, d2.toInt)
          case _ => throw IllegalArgumentException(s"Could not parse $line"))
    var pivot = AoCUtil.pivotMatrix(input)

    solvePartOne(pivot)
    solvePartTwo(pivot)

  private def solvePartOne(input: ArrayBuffer[ArrayBuffer[Int]]): Unit =
    val sorted = input.map(buf => buf.sorted)

    val totalDistance = (sorted(0) zip sorted(1))
      .map(pair => math.abs(pair._1 - pair._2))
      .sum
    println(s"Part One: the total distance is $totalDistance")

  private def solvePartTwo(input: ArrayBuffer[ArrayBuffer[Int]]): Unit =
    val col2counts = input(1).groupMapReduce(_.toInt)(_ => 1)(_ + _)

    val totalSimilarity = input.head
      .map(value =>
        if col2counts.contains(value) then
          (value * col2counts(value)) else 0)
      .sum

    println(s"Part Two: the total similarity is $totalSimilarity")
