package AoCLib

import AoCLib.Direction._
import AoCLib.RotationDirection._
import org.scalatest.funsuite.AnyFunSuite

class PositionTest extends AnyFunSuite:
  test("create") {
    val pDefault = Position()
    assert(pDefault.coord === Coord.origin)
    assert(pDefault.direction === N)

    val e = Position(Coord(5,5), E)
    assert(e.coord == Coord(5,5))
    assert(e.direction === E)
    var p = Position.withString(Coord(5,5), directionStr = "W")
    assert(p.coord == Coord(5, 5))
    assert(p.direction === W)
    try
      p = Position.withString(Coord(5,5), "Q")
      assert(e.coord == Coord(5, 5))
      assert(e.direction === W)
    catch
      case iae: IllegalArgumentException => println(iae.getMessage)
  }

  test("turning") {
    val noDir = Position(Coord.origin)
    assert(noDir.direction == N)
    assert(noDir.turn(CW).direction == E)

    var p = Position(Coord.origin, N)
    p = p.turn(CW)
    assert(p.direction == E)
    p = p.turn(CCW)
    assert(p.direction == N)
    for _ <- 1 to 5 do
      p = p.turn(CW)
    assert(p.direction == E)
  }

  test("moving") {
    var p = Position(Coord.origin, S)
    p = p.moveForward()
    assert(p.coord.x == 0)
    assert(p.coord.y == 1)
    p = p.moveForward(100)
    assert(p.coord.x == 0)
    assert(p.coord.y == 101)
  }