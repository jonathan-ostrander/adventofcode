package aoc
package y2022

object Day04 extends Day(4, 2022) {
  val pairs = input.map{ o =>
    val l = o.split(",").map { p =>
      val k = p.split("-").map(_.toInt).toList
      k.head -> k.last
    }.toList.sorted
    l.head -> l.last
  }

  override def partOne(): String = pairs.count { case ((fh, fl), (lh, ll)) => ll <= fl || fh == lh }.toString
  override def partTwo(): String = pairs.count { case ((_, fl), (lh, _)) => fl >= lh }.toString
}
