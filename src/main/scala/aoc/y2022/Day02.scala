package aoc
package y2022

/**
 * Win 6, Draw 3, Loss 0
 * A X Rock 1
 * B Y Paper 2
 * C Z Scissors 3 
 */
object Day02 extends Day(2, 2022) {
  def outcome(game: String): Int = game match {
    case "A X" => 4
    case "A Y" => 8
    case "A Z" => 3
    case "B X" => 1
    case "B Y" => 5
    case "B Z" => 9
    case "C X" => 7
    case "C Y" => 2
    case "C Z" => 6
    case _ => sys.error("this shouldn't happen")
  }

  def outcome2(game: String): Int = game.last match {
    case 'X' => game.head match {
      case 'A' => outcome("A Z")
      case 'B' => outcome("B X")
      case 'C' => outcome("C Y")
    }
    case 'Y' => game.head match {
      case 'A' => outcome("A X")
      case 'B' => outcome("B Y")
      case 'C' => outcome("C Z")
    }
    case 'Z' => game.head match {
      case 'A' => outcome("A Y")
      case 'B' => outcome("B Z")
      case 'C' => outcome("C X")
    }
  }

  override def partOne(): String = input.map(outcome).sum.toString
  override def partTwo(): String = input.map(outcome2).sum.toString
}
