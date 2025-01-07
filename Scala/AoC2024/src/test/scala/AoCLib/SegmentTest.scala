package AoCLib

import AoCLib.Direction._
import org.scalatest.funsuite.AnyFunSuite

class SegmentTest extends AnyFunSuite:
  test("create") {
    val seg = Segment(Coord.origin, Coord(1,0))
    assert(seg.from.x == 0)
    assert(seg.from.y == 0)
    assert(seg.to.x == 1)
    assert(seg.to.y == 0)
    assert(seg.length == 1)
  }

  test("directionality") {
    var segH = Segment(Coord.origin, Coord(1, 0))
    assert(segH.isHorizontal)
    assert(!segH.isVertical)
    assert(segH.direction.contains(E))
    segH = Segment(Coord.origin, Coord(-1, 0))
    assert(segH.isHorizontal)
    assert(segH.direction.contains(W))
    var segV = Segment(Coord.origin, Coord(0, 1))
    assert(segV.isVertical)
    assert(segV.direction.contains(S))
    segV = Segment(Coord.origin, Coord(0, -1))
    assert(segV.isVertical)
    assert(segV.direction.contains(N))

    var segDiag = Segment(Coord.origin, Coord(1,1))
    assert(!segDiag.isVertical)
    assert(!segDiag.isHorizontal)
    assert(segDiag.direction.contains(SE))
    segDiag = segDiag.copy(to = Coord(-1,1))
    assert(segDiag.direction.contains(SW))
    segDiag = segDiag.copy(to = Coord(-1,-1))
    assert(segDiag.direction.contains(NW))
    segDiag = segDiag.copy(to = Coord(1,-1))
    assert(segDiag.direction.contains(NE))
  }

  test("zero length") {
    var segZero = Segment(Coord(5,5), Coord(5,5))
    assert(segZero.direction.isEmpty)
    assert(segZero.length == 0)
  }