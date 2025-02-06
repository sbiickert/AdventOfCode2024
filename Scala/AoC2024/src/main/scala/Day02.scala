import AoCLib.AoCUtil
import scala.util.boundary, boundary.break

class Day02(day: Int, name: String) extends AoCLib.Solution(day, name):
  override def solve(test: Boolean, index: Int): Unit =
    val input = AoCUtil.readInput(AoCUtil.fileName(day, test))
    val reports = input.map(line => line.split(" ").map( _.toInt))

    val part1SafeCount = solvePartOne(reports)
    solvePartTwo(reports, part1SafeCount)

  def solvePartOne(input: List[Array[Int]]): Int =
    val trends = input.map(reportToTrend)
    val safeTrends = trends.filter(indexOfUnsafeValue(_) < 0)
    val count = safeTrends.size
    println(s"Part One: the total number of safe reports is $count")
    count

  def solvePartTwo(input: List[Array[Int]], part1SafeCount:Int): Unit =
    val trends = input.map(reportToTrend)
    val unsafeTrends = trends.filter(indexOfUnsafeValue(_) >= 0)
    val unsafeMadeSafe = unsafeTrends.filter(canBeMadeSafe)
    val part2SafeCount = unsafeMadeSafe.length
    val total = part1SafeCount + part2SafeCount
    println(s"Part Two: the total number of safe reports is $total")
  
  private def reportToTrend(report:Array[Int]): List[Int] =
    report.toList
      .sliding(2)
      .map(pair => pair(1) - pair.head)
      .toList

  private def mkReport(trend: List[Int]): Array[Int] =
    trend.scan(0)(_ + _).toArray
    
  private def modifyReport(report:Array[Int], indexToRemove:Int): Array[Int] =
    val before = report.take(indexToRemove)
    val after = report.slice(indexToRemove+1, report.length)
    before ++ after
    
  // Returns -1 if all values are safe
  private def indexOfUnsafeValue(trend: List[Int]): Int =
    if trend.head == 0 then return 0
    val isUpward = trend.head > 0
    if isUpward then
      trend.indexWhere(v => { v < 1 || v > 3 })
    else
      trend.indexWhere(v => { v < -3 || v > -1 })

  private def canBeMadeSafe(trend: List[Int]): Boolean =
    val badIndex = indexOfUnsafeValue(trend)
    val report = mkReport(trend)
    val imin = math.max(0, badIndex-1)
    val imax = badIndex+1
    val indexesToRemove = (imin to imax).toList
    indexesToRemove.map(i => reportToTrend(modifyReport(report, i)))
      .map(indexOfUnsafeValue)
      .contains(-1)
    