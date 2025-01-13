import AoCLib.AoCUtil
import scala.util.boundary, boundary.break

class Day02(day: Int, name: String) extends AoCLib.Solution(day, name):
  override def solve(test: Boolean, index: Int): Unit =
    val input = AoCUtil.readInput(AoCUtil.fileName(day, test))
    val reports = input.map(line => line.split(" ").map( _.toInt))

    solvePartOne(reports)
    solvePartTwo(reports)

  def solvePartOne(input: List[Array[Int]]): Unit =
    val trends = input.map(reportToTrends)
    val safeTrends = trends.filter(indexOfUnsafeValue(_) < 0)
    val count = safeTrends.size
    println(s"Part One: the total number of safe reports is $count")

  def solvePartTwo(input: List[Array[Int]]): Unit =
    var safeCount = 0
    for report <- input do
      val unsafeIndex = indexOfUnsafeValue(reportToTrends(report))
      if unsafeIndex < 0 then
        safeCount += 1
      else
        // Bad value is either at unsafeIndex-1, unsafeIndex or unsafeIndex+1
        boundary {
          for i <- -1 to 1 do
            val r0 = report.patch(unsafeIndex + i, Nil, 1)
            if indexOfUnsafeValue(reportToTrends(r0)) < 0 then
              safeCount += 1
              break()
        }

    println(s"Part Two: the total number of safe reports is $safeCount")

  private def reportToTrends(report:Array[Int]):List[Int] =
    val indexed = report.zipWithIndex
    val trends = indexed.map(reportElem =>
      val idx = reportElem(1)
      if idx > 0 then
        indexed(idx)(0) - indexed(idx - 1)(0)
      else
        0
    )
    trends.toList.tail // head is zero

  // Returns -1 if all values are safe
  private def indexOfUnsafeValue(trends: List[Int]): Int =
    if trends.head == 0 then return 0
    val isUpward = trends.head > 0
    if isUpward then
      trends.indexWhere(v => { v < 1 || v > 3 })
    else
      trends.indexWhere(v => { v < -3 || v > -1 })
