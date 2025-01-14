import AoCLib.AoCUtil
import scala.collection.mutable

class Day05(day: Int, name: String) extends AoCLib.Solution(day, name):
  private type RuleMap = mutable.HashMap[Int, Set[Int]]

  override def solve(test: Boolean, index: Int): Unit =
    val input = AoCUtil.readGroupedInput(AoCUtil.fileName(day, test))

    val rules = parseRules(input.head)
    val updates = input(1).map(line => line.split(",").map(_.toInt))

    solvePartOne(updates, rules)
    solvePartTwo(updates, rules)

  def solvePartOne(updates: List[Array[Int]], rules: RuleMap): Unit =
    val correct = updates.filter(isInTheCorrectOrder(_, rules))
    val sum = correct.map(update =>
        val mid = update.length / 2
        update(mid))
      .sum

    println(s"Part One: the sum of the mid values is $sum")

  def solvePartTwo(updates: List[Array[Int]], rules: RuleMap): Unit =
    val incorrect = updates.filter(!isInTheCorrectOrder(_, rules))
    val corrected = incorrect.map(fixOrder(_, rules))
    val sum = corrected.map(update =>
        val mid = update.length / 2
        update(mid))
      .sum
    println(s"Part Two: the sum of the mid values is $sum")

  def parseRules(input: Iterable[String]): RuleMap =
    val rules = mutable.HashMap.empty[Int, mutable.HashSet[Int]]
    val pairs = input.map(_.split("""\|""").map(_.toInt))
    for pair <- pairs do
      if !rules.contains(pair(1)) then
        rules(pair(1)) = mutable.HashSet.empty[Int]
      rules(pair(1)).addOne(pair(0))

    rules.map((k, v) => (k, v.toSet))

  def isInTheCorrectOrder(update: Array[Int], rules: RuleMap): Boolean =
    if update.length <= 1 then return true
    val v = update.head
    if rules.contains(v) then
      for valueThatHasToComeBefore <- rules(v) do
        if update.tail.contains(valueThatHasToComeBefore) then
          return false
    isInTheCorrectOrder(update.tail, rules)

  def fixOrder(update: Array[Int], rules: RuleMap): List[Int] =
    val sortFn: (Int, Int) => Boolean = (p1,p2) =>
      if rules.contains(p1) && rules(p1).contains(p2) then
        false
      else
        true

    update.toList.sortWith(sortFn)
