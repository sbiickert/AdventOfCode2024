package AoCLib

import scala.collection.mutable
import AoCLib.Direction.*

case class Extent(min: Coord, max: Coord):

  override def toString: String = s"[$min,$max]"

  def isValid: Boolean =
    min.x <= max.x && min.y <= max.y

  def expandToFit(coord: Coord): Extent =
    expandToFit( List(coord) )
    
  def expandToFit(coords: List[Coord]): Extent =
    if coords.isEmpty then return this
    val newMin = Coord(math.min(min.x, coords.head.x), math.min(min.y, coords.head.y))
    val newMax = Coord(math.max(max.x, coords.head.x), math.max(max.y, coords.head.y))
    Extent(newMin, newMax).expandToFit(coords.tail)


  def NW: Coord = min
  def NE: Coord = Coord(max.x, min.y)
  def SW: Coord = Coord(min.x, max.y)
  def SE: Coord = max

  val width: Int = max.x - min.x + 1
  val height: Int = max.y - min.y + 1
  val area: Int = width * height

  def allCoords: List[Coord] =
    val xs = (min.x to max.x).toList
    val ys = (min.y to max.y).toList
    AoCUtil.cartesian(ys,xs) // to get reading order
      .map((y:Any, x:Any) => (x.asInstanceOf[Int], y.asInstanceOf[Int]))
      .map((x, y) => Coord(x,y))

  def contains(coord: Coord): Boolean =
    min.x <= coord.x && coord.x <= max.x && min.y <= coord.y && coord.y <= max.y

  def inset(inset: Int): Extent =
    Extent.fromInts(min.x + inset, min.y + inset, max.x - inset, max.y - inset)

  def intersect(other: Extent): Option[Extent] =
    val commonMinX = math.max(min.x, other.min.x)
    val commonMaxX = math.min(max.x, other.max.x)
    if commonMaxX < commonMinX then return None
    val commonMinY = math.max(min.y, other.min.y)
    val commonMaxY = math.min(max.y, other.max.y)
    if commonMaxY < commonMinY then return None
    Some(Extent.fromInts(commonMinX, commonMinY, commonMaxX, commonMaxY))

  def union(other: Extent): List[Extent] =
    if this == other then return List(this)

    val intersectResult = this.intersect(other)
    if intersectResult.isEmpty then return List(this, other)

    val mResult = mutable.ArrayBuffer.empty[Extent]
    val eInt = intersectResult.get
    mResult.addOne(eInt)

    for e <- List(this, other) do
      if e != eInt then
        // There are eight "edges" & "corners"
        // West edge
        if e.contains(eInt.NW.offset(Direction.W)) then
          val newE = Extent.fromInts(e.NW.x, eInt.NW.y, eInt.NW.x-1, eInt.SW.y)
          mResult.addOne(newE)
        // NW corner
        if e.contains(eInt.NW.offset(Direction.NW)) then
          val newE = Extent.fromInts(e.NW.x, e.NW.y, eInt.NW.x-1, eInt.NW.y-1)
          mResult.addOne(newE)
        // North edge
        if e.contains(eInt.NW.offset(Direction.N)) then
          val newE = Extent.fromInts(eInt.NW.x, e.NW.y, eInt.NE.x, eInt.NE.y-1)
          mResult.addOne(newE)
        // NE corner
        if e.contains(eInt.NE.offset(Direction.NE)) then
          val newE = Extent.fromInts(eInt.NE.x+1, e.NE.y, e.SE.x, eInt.NE.y-1)
          mResult.addOne(newE)
        // East edge
        if e.contains(eInt.SE.offset(Direction.E)) then
          val newE = Extent.fromInts(eInt.SE.x+1, eInt.NE.y, e.SE.x, eInt.SE.y)
          mResult.addOne(newE)
        // SE corner
        if e.contains(eInt.SE.offset(Direction.SE)) then
          val newE = Extent.fromInts(eInt.SE.x+1, eInt.SE.y + 1, e.SE.x, e.SE.y)
          mResult.addOne(newE)
        // South edge
        if e.contains(eInt.SE.offset(Direction.S)) then
          val newE = Extent.fromInts(eInt.SW.x, eInt.SW.y+1, eInt.SE.x, e.SW.y)
          mResult.addOne(newE)
        // SW corner
        if e.contains(eInt.SW.offset(Direction.SW)) then
          val newE = Extent.fromInts(e.SW.x, eInt.SW.y+1, eInt.SW.x-1, e.SW.y)
          mResult.addOne(newE)

    mResult.toList

object Extent:
  def fromInts(xmin: Int, ymin: Int, xmax: Int, ymax: Int): Extent =
    Extent.fromCoords(List(Coord(xmin, ymin), Coord(xmax, ymax)))

  def fromCoords(coords: List[Coord]): Extent =
    val e1 = Extent(coords.head, coords.head)
    e1.expandToFit(coords.tail)