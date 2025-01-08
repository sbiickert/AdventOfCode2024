package AoCLib

abstract class Solution(day: Int, name: String):
  def solve(test: Boolean, index: Int = 0): Unit
    println(s"Solving Day $day, $name") 
