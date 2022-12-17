package aoc
package y2022

object Day05 extends Day(5, 2022) {
  val stacksInput = input.takeWhile(!_.startsWith(" 1 "))
  val instructionsInput = input.dropWhile(!_.startsWith("move"))

  val emptyStacks = List.fill((stacksInput.head.length / 4) + 1)(List.empty[Char])
  val stacks = stacksInput.foldRight[List[List[Char]]](emptyStacks) {
    case (line, curStacks) =>
      line.grouped(4).map(_.drop(1).head).zip(curStacks).map {
        case (' ', stack) => stack
        case (char, stack) => char :: stack
      }.toList
  }.toIndexedSeq

  case class Instruction(fromStack: Int, toStack: Int, toMove: Int) {
    def rearrange(stacks: IndexedSeq[List[Char]], altogether: Boolean = false): IndexedSeq[List[Char]] = {
      val stackArray = stacks.toArray

      def loop(movesLeft: Int): IndexedSeq[List[Char]] =
        if (movesLeft == 0) stackArray.toIndexedSeq
        else {
          stackArray(toStack) = stackArray(fromStack).head :: stackArray(toStack)
          stackArray(fromStack) = stackArray(fromStack).tail
          loop(movesLeft - 1)
        }

      if (!altogether) loop(toMove)
      else {
        stackArray(toStack) = stackArray(fromStack).take(toMove) ++ stackArray(toStack)
        stackArray(fromStack) = stackArray(fromStack).drop(toMove)
        stackArray.toIndexedSeq
      }
    }
  }

  val instructions = instructionsInput.map { line =>
    val split = line.split(" ").toList
    Instruction(
      fromStack = split.drop(3).head.toInt - 1,
      toStack = split.drop(5).head.toInt - 1,
      toMove = split.drop(1).head.toInt,
    )
  }

  override def partOne(): String =
    instructions.foldLeft(stacks) { case (ss, instruction) => instruction.rearrange(ss) }.map(_.head).mkString
  
  override def partTwo(): String =
    instructions.foldLeft(stacks) { case (ss, instruction) => instruction.rearrange(ss, true) }.map(_.head).mkString
  
}
