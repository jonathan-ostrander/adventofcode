package aoc
package y2022

object Day03 extends Day(3, 2022) {
  def value(char: Char): Int =
    if (char.isUpper) char.toInt - 'A'.toInt + 1 + 'z'.toInt - 'a'.toInt + 1
    else char.toInt - 'a'.toInt + 1

  override def partOne(): String =
    input.map(s => value(s.take(s.length / 2).toSet.intersect(s.drop(s.length / 2).toSet).head)).sum.toString
  override def partTwo(): String =
    input.map(_.toSet).grouped(3).map(g => value(g.reduce[Set[Char]]{ case (a, b) => a.intersect(b) }.head)).sum.toString
}
