package AoCLib

import AoCLib.AdjacencyRule.Rook
import scala.collection.mutable

trait GridGlyph:
  def glyph:String

class Grid(var default: Any = ".", var rule: AdjacencyRule = Rook):
  var data = mutable.Map.empty[Coord, Any]
  var extent: Option[Extent] = None

  def get(key: Coord):Any =
    if data.contains(key) then
      data(key)
    else
      default

  def getString(key: Coord): String =
    val value = get(key)
    value match
      case str: String => str
      case gg: GridGlyph => gg.glyph
      case _ => value.toString

  def set(key: Coord, value: Any) =
    data(key) = value

    if extent.isEmpty then
      extent = Some(Extent.fromCoords(List(key)))
    else
      extent = Some(extent.get.expandToFit(key))

  def clear(key: Coord, resetExtent: Boolean = false) =
    data.remove(key)
    if resetExtent then this.resetExtent

  def resetExtent =
    extent = Some(Extent.fromCoords(data.keys.toList))

  def coords(withValue: Option[String] = None): Iterable[Coord] =
    if withValue.isEmpty then
      data.keys
    else
      val str = withValue.get
      data.keys.filter( getString(_) == str )

  def histogram(includeUnset: Boolean = false): Map[String, Int] =
    if extent.isEmpty then return Map.empty[String, Int]
    val ext = extent.get
    val coordsToSum = if includeUnset then ext.allCoords else coords()

    coordsToSum.toList
      .map((c:Coord) => this.getString(c))
      .groupMapReduce(_.toString)(_ => 1)(_ + _)

  def neighbors(coord: Coord): List[Coord] =
    coord.getAdjacentCoords(rule)

  def print(markers: Option[Map[Coord, String]] = None) =
    println(sprint(markers))

  def sprint(markers: Option[Map[Coord, String]] = None): String =
    if extent.isEmpty then return ""

    val ext = extent.get
    var sb = StringBuilder()

    for y <- (ext.min.y to ext.max.y) do
      val row = mutable.ArrayBuffer.empty[String]
      for x <- (ext.min.x to ext.max.x) do
        val c = Coord(x,y)
        var glyph = getString(c)
        if markers.nonEmpty && markers.get.contains(c) then
          glyph = markers.get(c)
        row.addOne(glyph)
      row.addOne("\n")
      sb ++= row.mkString(" ")
    sb.mkString
