package aoc
package y2022

import scala.collection.mutable

object Day08 extends Day(8, 2022) {
  val rows = input.zipWithIndex.map {
    case (row, rowIndex) => row.map(_.toString.toInt).zipWithIndex.map {
      case (tree, columnIndex) => (tree, (rowIndex, columnIndex))
    }.toList
  }.toList
  val orientations = List(
    rows,
    rows.map(_.reverse),
    rows.transpose,
    rows.transpose.map(_.reverse),
  )

  override def partOne(): String = {
    val visible = mutable.Set.empty[(Int, Int)]

    def addToVisible(trees: List[(Int, (Int, Int))], maxHeight: Int = -1): Unit = trees match {
      case Nil => ()
      case (height, coor) :: tail =>
        if (height > maxHeight) visible.add(coor)
        addToVisible(tail, height.max(maxHeight))
    }

    orientations.flatten.foreach(r => addToVisible(r))

    visible.size.toString
  }
  
  override def partTwo(): String = {
    def treeScore(coor: (Int, Int)): Int = {
      orientations
        .flatten
        .filter(_.exists(_._2 == coor))
        .map(_.dropWhile(_._2 != coor))
        .map {
          case Nil => sys.error("missing coordinate")
          case (height, _) :: tail =>
            val canSee = tail.takeWhile(_._1 < height)
            if (canSee.size == tail.size) canSee.size
            else canSee.size + 1
        }.product
    }
    rows.flatten.map(row => treeScore(row._2)).max.toString
  }
  
}
