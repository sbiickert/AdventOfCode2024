package AoCLib

enum Direction:
  case N, NE, E, SE, S, SW, W, NW

object Direction:
  def fromString(str: String): Direction =
    val ucStr = str.toUpperCase
    ucStr match
      case "N" => N
      case "^" => N
      case "U" => N
      case "NE" => NE
      case "E" => E
      case ">" => E
      case "R" => E
      case "SE" => SE
      case "S" => S
      case "v" => S
      case "D" => S
      case "SW" => SW
      case "W" => W
      case "<" => W
      case "L" => W
      case "NW" => NW
      case _ => throw IllegalArgumentException(s"$str is not a valid Direction")