package AoCLib

import org.scalatest.funsuite.AnyFunSuite

private class GridObject(id: Int, kind: String, hp: Int) extends GridGlyph:
  override def glyph: String = kind.substring(0, 1)

class GridTest extends AnyFunSuite:
  test("grid") {
    val grid = Grid()
    assert(grid.default == ".")
    assert(grid.rule == AdjacencyRule.Rook)
    assert(grid.extent.isEmpty)

    val c = Array(Coord(1, 1), Coord(2, 2), Coord(3, 3), Coord(4, 4),
      Coord(1, 4), Coord(2, 4), Coord(3, 4))

    grid.set(c(0), "A")
    grid.set(c(1), "B")
    grid.set(c(3), "D")

    assert(grid.get(c(0)) == "A")
    assert(grid.get(c(1)) == "B")
    assert(grid.get(c(2)) == grid.default)
    assert(grid.get(c(3)) == "D")

    grid.set(c(4), GridObject(1, "Elf", 100))
    grid.set(c(5), GridObject(2, "Goblin", 95))
    grid.set(c(6), GridObject(3, "Santa", 1000))

    assert(grid.getString(c(4)) == "E")
    assert(grid.getString(c(5)) == "G")
    assert(grid.getString(c(6)) == "S")

    assert(grid.extent.nonEmpty)
    val ext = grid.extent.get
    assert(ext.min == Coord(1, 1))
    assert(ext.max == Coord(4, 4))

    assert(grid.coords().size == 6)
    val matching = grid.coords(Some("B"))
    assert(matching.size == 1)
    assert(matching.head == c(1))

    // Histogram
    grid.set(c(2), "B")
    val hist = grid.histogram()
    assert(hist("A") == 1 && hist("B") == 2 && !hist.contains("."))
    var histWithDefault = grid.histogram(true)
    assert(histWithDefault("A") == 1 && histWithDefault("B") == 2 && histWithDefault(".") == 9)

    // Printing
    grid.print()
    var gridString = grid.sprint()
    assert(gridString == "A . . . \n. B . . \n. . B . \nE G S D \n")

    val markers = Map(Coord(4, 1) -> "*")
    grid.print(Some(markers))
    gridString = grid.sprint(Some(markers))
    assert(gridString == "A . . * \n. B . . \n. . B . \nE G S D \n")

    // Clearing
    grid.clear(c(2))
    assert(grid.get(c(2)) == ".")
    val originalExt = grid.extent.get
    grid.set(Coord(100, 100), "X")
    assert(grid.extent.get.max == Coord(100, 100))
    grid.clear(Coord(100, 100), resetExtent = true)
    assert(grid.extent.get == originalExt)
  }