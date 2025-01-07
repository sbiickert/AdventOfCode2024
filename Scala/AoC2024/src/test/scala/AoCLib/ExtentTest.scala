package AoCLib

import org.scalatest.funsuite.AnyFunSuite

class ExtentTest extends AnyFunSuite:
  test("create") {
    val c0 = Coord(-1, 1)
    val c1 = Coord(2, 8)
    val c2 = Coord(3, 3)
    val c3 = Coord(4, 4)
    var cList = List(c2, c1, c0)

    val e0 = Extent.fromCoords( List(c0, c1) )
    val e1 = Extent.fromCoords( cList )
    val e2 = e1.expandToFit( c3 )

    assert(e0 == Extent.fromInts(-1, 1, 2, 8))
    assert(e1 == Extent.fromInts(-1, 1, 3, 8))
    assert(e2 == Extent.fromInts(-1, 1, 4, 8))
  }

  test("size/bounds") {
    val e0 = Extent.fromInts(-1, 1, 2, 8)
    val e1 = Extent.fromInts(-1, 1, 3, 8)
    val e2 = Extent.fromInts(-1, 1, 4, 8)

    assert(e0.width == 4, e0.height == 8)
    assert(e1.area == 40)
    assert(e2.NW == Coord(-1, 1))
    assert(e2.SE == Coord(4,8))
  }

  test("all coords") {
    val e1 = Extent.fromInts(-1, 1, 3, 8)
    val coords = e1.allCoords
    assert(e1.area == coords.size)
    // check "reading order"
    assert(coords.head == Coord(-1,1))
    assert(coords(1) == Coord(0,1))
    assert(coords.last == Coord(3,8))
  }

  test("topology") {
    val e0 = Extent.fromInts(-1, 1, 2, 8)
    assert(e0.inset(1) == Extent.fromInts(0, 2, 1, 7))
    assert(e0.inset(2) == Extent.fromInts(0, 3, 1, 6)) // xmin and xmax cross each other
    assert(e0.inset(-1) == Extent.fromInts(-2, 0, 3, 9))

    val e3 = Extent.fromInts(1, 1, 10, 10).intersect(Extent.fromInts(5, 5, 12, 12))
    val e4 = Extent.fromInts(1, 1, 10, 10).intersect(Extent.fromInts(5, 5, 7, 7))
    val e5 = Extent.fromInts(1, 1, 10, 10).intersect(Extent.fromInts(1, 1, 12, 2))
    val e6 = Extent.fromInts(1, 1, 10, 10).intersect(Extent.fromInts(11, 11, 12, 12))
    val e7 = Extent.fromInts(1, 1, 10, 10).intersect(Extent.fromInts(1, 10, 10, 20))

    assert(e3.contains(Extent.fromInts(5,5,10,10)))
    assert(e4.contains(Extent.fromInts(5,5,7,7)))
    assert(e5.contains(Extent.fromInts(1,1,10,2)))
    assert(e6.isEmpty)
    assert(e7.contains(Extent.fromInts(1,10,10,10)))
  }

  test("union") {
    val base = Extent.fromInts(1,1,10,10)
    var products = base.union(Extent.fromInts(5,5,12,12))
    var expected = List(Extent.fromInts(5,5,10,10),Extent.fromInts(1,5,4,10),Extent.fromInts(1,1,4,4),Extent.fromInts(5,1,10,4),Extent.fromInts(11,5,12,10),Extent.fromInts(11,11,12,12),Extent.fromInts(5,11,10,12))
    assert(products.size == expected.size)
    assert(products == expected)

    products = base.union(Extent.fromInts(5,5,7,7))
    expected = List(Extent.fromInts(5,5,7,7),Extent.fromInts(1,5,4,7),Extent.fromInts(1,1,4,4),Extent.fromInts(5,1,7,4),Extent.fromInts(8,1,10,4),Extent.fromInts(8,5,10,7),Extent.fromInts(8,8,10,10),Extent.fromInts(5,8,7,10),Extent.fromInts(1,8,4,10))
    assert(products.size == expected.size)
    assert(products == expected)

    products = base.union(Extent.fromInts(1,1,12,2))
    expected = List(Extent.fromInts(1,1,10,2),Extent.fromInts(1,3,10,10),Extent.fromInts(11,1,12,2))
    assert(products.size == expected.size)
    assert(products == expected)

    products = base.union(Extent.fromInts(11,11,12,12))
    expected = List(base,Extent.fromInts(11,11,12,12))
    assert(products.size == expected.size)
    assert(products == expected)

    products = base.union(Extent.fromInts(1,10,10,20))
    expected = List(Extent.fromInts(1,10,10,10),Extent.fromInts(1,1,10,9),Extent.fromInts(1,11,10,20))
    assert(products.size == expected.size)
    assert(products == expected)
  }

