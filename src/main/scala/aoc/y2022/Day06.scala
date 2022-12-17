package aoc
package y2022

object Day06 extends Day(6, 2022) {
  def find(n: Int): Int =
    (input.head.sliding(n).zipWithIndex.find(_._1.toSet.size == n).head._2 + n)

  override def partOne(): String =
    find(4).toString
  
  override def partTwo(): String =
    find(14).toString
}
