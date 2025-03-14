import AoCLib.AoCUtil

import java.time.LocalDateTime
import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable

class Day07(day: Int, name: String) extends AoCLib.Solution(day, name):
  override def solve(test: Boolean, index: Int): Unit =
    val input = AoCUtil.readInput(AoCUtil.fileName(day, test))

    // Lists with the result at the head, and operands following in the tail
    val numbers = input.map(line => line.split(""":? """).map(_.toLong))

    solvePartOne(numbers)
    solvePartTwo(numbers)

  def solvePartOne(input: List[Array[Long]]): Unit =
    // Parallel processing
    val possibles = input.par.filter(isValuePossible1)
    val sum = possibles.map(_.head).sum
    println(s"Part One: the sum of possibles is $sum")

  def solvePartTwo(input: List[Array[Long]]): Unit =
    // Parallel processing
    val possibles = input.par.filter(isValuePossible2)
    val sum = possibles.map(_.head).sum
    println(s"Part Two: the sum of possibles is $sum")

  private def isValuePossible1(formula: Array[Long]): Boolean =
    if formula.length == 2 then return formula(0) == formula(1)
    val mulArray = mutable.ArrayBuffer(formula.head, formula(1) * formula(2)).addAll(formula.slice(3, formula.length))
    if isValuePossible1(mulArray.toArray) then return true
    val addArray = mutable.ArrayBuffer(formula.head, formula(1) + formula(2)).addAll(formula.slice(3, formula.length))
    isValuePossible1(addArray.toArray)

  private def isValuePossible2(formula: Array[Long]): Boolean =
    if formula.length == 2 then return formula(0) == formula(1)
    val catArray = mutable.ArrayBuffer(formula.head, (formula(1).toString + formula(2).toString).toLong)
      .addAll(formula.slice(3, formula.length))
    if isValuePossible2(catArray.toArray) then return true
    val mulArray = mutable.ArrayBuffer(formula.head, formula(1) * formula(2)).addAll(formula.slice(3, formula.length))
    if isValuePossible2(mulArray.toArray) then return true
    val addArray = mutable.ArrayBuffer(formula.head, formula(1) + formula(2)).addAll(formula.slice(3, formula.length))
    isValuePossible2(addArray.toArray)
