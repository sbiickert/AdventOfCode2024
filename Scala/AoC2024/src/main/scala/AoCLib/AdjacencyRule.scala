package AoCLib

import AoCLib.Direction._

enum AdjacencyRule:
  case Rook, Bishop, Queen
  
  def directions: List[Direction] =
    this match
      case Rook => List(N, E, S, W)
      case Bishop => List(NE, SE, SW, NW)
      case Queen => List(N, NE, E, SE, S, SW, W, NW)