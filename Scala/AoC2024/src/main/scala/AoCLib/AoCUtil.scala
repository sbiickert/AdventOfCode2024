package AoCLib

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable

class AoCUtil()

object AoCUtil:
  val sep: String = System.getProperty("file.separator")
  val inputFolder: String =
    val components = List(System.getProperty("user.dir"), "..", "..", "Input")
    components.mkString(sep)

  def fileName(day: Int, test: Boolean): String =
    val challengeTest = if test then "test" else "challenge"
    "day%02d_%s.txt".format(day, challengeTest)

  def readInput(fileName: String, removeEmptyLines: Boolean = true): List[String] =
    val bufferedSource = Source.fromFile(List(inputFolder, fileName).mkString(sep))
    val lines = bufferedSource.getLines()
    var input = lines.toList
    if removeEmptyLines then
      input = input.filter(_.nonEmpty)
    bufferedSource.close
    input

  def readGroupedInput(fileName: String): List[List[String]] =
    val input = readInput(fileName, removeEmptyLines = false)
    var groups: mutable.ArrayBuffer[List[String]] = mutable.ArrayBuffer.empty[List[String]]
    var group: mutable.ArrayBuffer[String] = mutable.ArrayBuffer.empty[String]

    for (line <- input)
      if line.nonEmpty then
        group.addOne(line)
        //println(line)
      else
        val temp = group.toList
        //println(s"Add $temp to $groups")
        groups.addOne(group.toList)
        group = mutable.ArrayBuffer.empty[String]

    if group.nonEmpty then
      groups.addOne(group.toList)

    groups.toList

  // Returns the cartesian product of two Lists.
  // Like python itertools.product
  def cartesian[A](aList: List[A], bList: List[A]): List[(A, A)] =
    aList.flatMap(a => bList.map(b => (a, b)))

  // Takes an array with at least two dimensions and swaps rows and columns
  // e.g. a 2 x 10 matrix becomes a 10 x 2 matrix
  def pivotMatrix[A](source: Iterable[Iterable[A]]): mutable.ArrayBuffer[mutable.ArrayBuffer[A]] =
    val pivot = mutable.ArrayBuffer.empty[mutable.ArrayBuffer[A]]
    for _ <- 0 until source.head.size do
      pivot.addOne(mutable.ArrayBuffer.empty[A])

    for row <- source do
      var col = 0
      for value <- row do
        pivot(col).addOne(value)
        col += 1

    pivot

  // Greatest Common Divisor between x and y
  def gcd(x: Long, y: Long): Long =
    var a = 0L
    var b = math.max(x, y)
    var r = math.min(x, y)
    while r != 0 do
      a = b
      b = r
      r = a % b
    b
  
  // Least Common Multiple among an arbitrary list of Longs
  @tailrec
  def lcm(values: List[Long]): Long =
    if values.isEmpty then return 0L
    var running:Long = values.head
    if values.size == 1 then return running

    val next = values.tail.head
    running = running / gcd(running, next) * next
    val nextValues:List[Long] = List(List(running), values.tail.tail).flatten()
    lcm(nextValues)
