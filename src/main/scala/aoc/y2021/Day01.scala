package aoc
package y2021

object Day01 extends Day(1, 2021) {
  val ints = input.map(_.toInt)

  def slidingIncrease(windowSize: Int): Int =
    ints
      .sliding(windowSize + 1)
      .count(window => window.head < window.last)

  override def partOne(): String = slidingIncrease(1).toString

  override def partTwo(): String = slidingIncrease(3).toString
}
