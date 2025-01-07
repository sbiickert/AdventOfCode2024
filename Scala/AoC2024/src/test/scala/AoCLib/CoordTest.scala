package AoCLib

import AoCLib.AdjacencyRule._
import AoCLib.Direction.*
import org.scalatest.funsuite.AnyFunSuite
//import org.scalactic.TolerantNumerics
import org.scalactic.Tolerance.convertNumericToPlusOrMinusWrapper

class CoordTest extends AnyFunSuite:
  test("create") {
    val c = Coord(10,30)
    assert(c.x == 10)
    assert(c.y == 30)
    assert(c.toString == "[10,30]")
    val copied = c.copy(y = -40)
    assert(copied.x == 10)
    assert(copied.y == -40)
    assert(copied.toString == "[10,-40]")
    val fromStr = Coord.fromString(copied.toString)
    assert(fromStr === Some(Coord(10,-40)))
  }

  test("equality") {
    val c1 = Coord(10,30)
    val c2 = c1.copy()
    assert(c1 == c2)
    val c3 = Coord(5,20)
    assert(c1 != c3)
    val cOrigin = Coord(0,0)
    assert(cOrigin == Coord.origin)
  }

  test("add/subtract") {
    val c1 = Coord(10,30)
    val c2 = Coord(5,20)
    val sum = c1 + c2
    assert(sum.x == 15)
    assert(sum.y == 50)
    var delta = c1.delta(c2)
    assert(delta.x == -5, delta.y == -10)
  }

  test("distance") {
    val c1 = Coord(10,30)
    val c2 = Coord(5,20)
    val md = c1.manhattanDistanceTo(c2)
    assert(md === 15)
    val d = c1.distanceTo(c2)
    assert(d === 11.18 +- 0.01)
  }

  test("offsets") {
    assert(Coord.offset(N) === Coord(0,-1))
    assert(Coord.offset("N") === Coord(0,-1))
    assert(Coord.offset("^") === Coord(0,-1))
    assert(Coord.offset(SW) === Coord(-1,1))
    try
      assert(Coord.offset("?") === Coord(0, 0))
    catch
      case iae: IllegalArgumentException => println(iae.getMessage)
  }

  test("adjacency") {
    val cHorz = Coord(1,0)
    assert(cHorz.isAdjacentTo(Coord.origin)) // With default Rook rule
    assert(!cHorz.isAdjacentTo(Coord.origin, Bishop))
    assert(cHorz.isAdjacentTo(Coord.origin, Queen))
    val cDiag = Coord(1,1)
    assert(!cDiag.isAdjacentTo(Coord.origin)) // With default Rook rule
    assert(cDiag.isAdjacentTo(Coord.origin, Bishop))
    assert(cDiag.isAdjacentTo(Coord.origin, Queen))
    val cFar = Coord(2,0)
    assert(!cFar.isAdjacentTo(Coord.origin, Queen))

    val rookAdj = Coord.origin.getAdjacentCoords(Rook)
    assert(rookAdj.size == 4)
    var bishopAdj = Coord.origin.getAdjacentCoords(Bishop)
    assert(bishopAdj.size == 4)
    var queenAdj = Coord.origin.getAdjacentCoords(Queen)
    assert(queenAdj.size == 8)
  }