package AoCLib

import scala.io.Source
import scala.collection.mutable

class AoCUtil()

object AoCUtil:
  val year = 2024
  val inputFolder = s"/Users/sjb/Developer/Advent of Code/$year/AdventOfCode$year/Input"

  def readInput(fileName: String, removeEmptyLines: Boolean = true): List[String] =
    val bufferedSource = Source.fromFile(s"$inputFolder/$fileName")
    //println(s"$inputFolder/$fileName")
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

