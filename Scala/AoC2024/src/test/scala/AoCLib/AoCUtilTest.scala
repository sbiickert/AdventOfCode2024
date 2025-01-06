package AoCLib

import org.scalatest.funsuite.AnyFunSuite

class AoCUtilTest extends AnyFunSuite:
  val fileName = "day00_test.txt"
  test("readInput") {
    val inputWithEmpty = AoCUtil.readInput(fileName, removeEmptyLines = false)
    assert(inputWithEmpty.size == 10)
    assert(inputWithEmpty.head == "G0, L0")
    assert(inputWithEmpty(3).isEmpty)
    val inputWithoutEmpty = AoCUtil.readInput(fileName)
    assert(inputWithoutEmpty.size == 8)
    assert(inputWithoutEmpty(3) == "G1, L0")
  }

  test("readGroupedInput") {
    val input = AoCUtil.readGroupedInput(fileName)
    assert(input.size == 3)
    assert(input.map(_.size) == List(3,2,3))
    assert(input(2)(2) == "G2, L2")
  }