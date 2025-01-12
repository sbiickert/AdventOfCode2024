import AoCLib.AoCUtil

class Day02(day: Int, name: String) extends AoCLib.Solution(day, name):
  override def solve(test: Boolean, index: Int): Unit =
    val input = AoCUtil.readInput(AoCUtil.fileName(day, test))
    val reports = input.map(line => line.split(" ").map( _.toInt))

    solvePartOne(reports)

  def solvePartOne(input: List[Array[Int]]): Unit =
    val safeReports = input.map( _.toArray)
      .filter(isReportSafe)
      .map(_.toList)
    println(safeReports)

    val count = safeReports.size
    println(s"Part One: the total number of safe reports is $count")

  def isReportSafe(report: Array[Int]): Boolean =
    val indexed = report.zipWithIndex
    var trends = indexed.map(reportElem =>
      val idx = reportElem(1)
      if idx > 0 then
        indexed(idx)(0) - indexed(idx-1)(0)
      else
        0
    )
    trends = trends.tail
    println(trends.toList)
    if trends.head == 0 then return false
    val isUpward = trends.head > 0
    if isUpward then
      !trends.exists(v => {v < 1 || v > 3})
    else
      !trends.exists(v => {v > -1 || v < -3})

  def isReportUnsafe(report: Array[Int]): Boolean = !isReportSafe(report)