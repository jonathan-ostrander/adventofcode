package aoc
package y2022

object Day09 extends Day(9, 2022) {
  case class Coordinates(x: Int, y: Int) {
    def moveTowards(other: Coordinates): Coordinates = {
      if ((x - other.x).abs > 1) Coordinates(x + (if (x > other.x) -1 else 1), y + (if (y > other.y) -1 else if (other.y > y) 1 else 0))
      else if ((y - other.y).abs > 1) Coordinates(x + (if (x > other.x) -1 else if (other.x > x) 1 else 0), y + (if (y > other.y) -1 else 1))
      else this
    }
  }

  sealed abstract class Direction(delta: Coordinates) {
    def x: Int = delta.x
    def y: Int = delta.y
  }
  case object Right extends Direction(Coordinates(1, 0))
  case object Left extends Direction(Coordinates(-1, 0))
  case object Up extends Direction(Coordinates(0, 1))
  case object Down extends Direction(Coordinates(0, -1))

  object Direction {
    def parse(char: Char): Direction = char match {
      case 'R' => Right
      case 'L' => Left
      case 'U' => Up
      case 'D' => Down
      case _ => sys.error(s"Unrecognized direction $char")
    }
  }

  case class Move(direction: Direction, distance: Int)

  object Move {
    def parse(line: String): Move = Move(Direction.parse(line.head), line.drop(2).toInt)
  }

  val moves = input.map(Move.parse)

  def moveAround(knots: List[Coordinates], visited: Set[Coordinates], movesLeft: List[Move]): Set[Coordinates] =
    movesLeft match {
      case Nil => visited
      case Move(dir, 0) :: tail => moveAround(knots, visited, tail)
      case Move(dir, distance) :: tail =>
        val newHead = Coordinates(knots.head.x + dir.x, knots.head.y + dir.y)
        val newKnots = knots.tail.foldLeft[List[Coordinates]](newHead :: Nil) {
          case (curHead :: rest, next) => next.moveTowards(curHead) :: curHead :: rest
          case (Nil, _) => sys.error("unreachable")
        }.reverse
        moveAround(newKnots, visited + newKnots.last, Move(dir, distance - 1) :: tail)
    }

  override def partOne(): String =
    moveAround(List.fill(2)(Coordinates(0, 0)), Set.empty, moves).size.toString
  
  override def partTwo(): String =
    moveAround(List.fill(10)(Coordinates(0, 0)), Set.empty, moves).size.toString
}
