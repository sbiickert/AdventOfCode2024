package AoCLib

enum RotationDirection:
  case CW, CCW

  // For turning
  def step:Int =
    this match
      case CW  =>  1
      case CCW => -1

object RotationDirection:
  def fromString(str: String): Option[RotationDirection] =
    val ucStr = str.toUpperCase
    ucStr match
      case "CW" =>  Some(CW)
      case "R" =>   Some(CW)
      case "CCW" => Some(CCW)
      case "L" =>   Some(CCW)
      case _ => None