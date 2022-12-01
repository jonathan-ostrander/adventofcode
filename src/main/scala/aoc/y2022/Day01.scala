package aoc
package y2022

object Day01 extends Day(1, 2022) {
  val elves = input.foldLeft[List[List[Int]]](Nil) {
    case (Nil, "") => Nil
    case (Nil, next) => List(next.toInt) :: Nil
    case (list, "") => Nil :: list
    case (head :: tail, next) => (next.toInt :: head) :: tail
  }

  val totals = elves.map(_.sum).sorted.reverse

  override def partOne(): String = totals.head.toString
  
  override def partTwo(): String = totals.take(3).sum.toString
}
