package aoc
package y2021

import scala.annotation.tailrec
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import scala.collection.mutable

object Day24 extends Day(24, 2021) {
  sealed trait VarOrInt {
    override def toString() = this match {
      case Variable(value) => value.toString()
      case Integer(value)  => value.toString()
    }
  }
  case class Variable(value: Char) extends VarOrInt
  case class Integer(value: Int) extends VarOrInt

  sealed trait Instruction {
    def apply(state: Map[Char, Long], inp: Option[Int]): Map[Char, Long]
  }
  case class Inp(variable: Char) extends Instruction {
    override def apply(
        state: Map[Char, Long],
        inp: Option[Int]
    ): Map[Char, Long] = {
      state + (variable -> inp.getOrElse(0).toLong)
    }
  }

  abstract class Operation(
      a: Char,
      b: VarOrInt,
      f: (Long, Long) => Long,
      symbol: Char
  ) extends Instruction {
    override def apply(
        state: Map[Char, Long],
        inp: Option[Int]
    ): Map[Char, Long] = {
      val second = b match {
        case Variable(value) => state.getOrElse(value, 0L)
        case Integer(value)  => value
      }
      val first = state.getOrElse(a, 0L)
      val result = f(first, second)
      state + (a -> result)
    }
  }
  case class Add(a: Char, b: VarOrInt) extends Operation(a, b, _ + _, '+')
  case class Mul(a: Char, b: VarOrInt) extends Operation(a, b, _ * _, '*')
  case class Div(a: Char, b: VarOrInt) extends Operation(a, b, _ / _, '/')
  case class Mod(a: Char, b: VarOrInt)
      extends Operation(
        a,
        b,
        (a, b) => if (a < 0 || b <= 0) sys.error("bad") else a % b,
        '%'
      )
  case class Eql(a: Char, b: VarOrInt)
      extends Operation(a, b, (a, b) => if (a == b) 1 else 0, '=')

  def parseInstruction(s: String): Instruction = s.split(" ").toList match {
    case "inp" :: c :: Nil => Inp(c.head)
    case "add" :: c :: v :: Nil =>
      Add(
        c.head,
        if (v.exists(_.isDigit)) Integer(v.toInt) else Variable(v.head)
      )
    case "mul" :: c :: v :: Nil =>
      Mul(
        c.head,
        if (v.exists(_.isDigit)) Integer(v.toInt) else Variable(v.head)
      )
    case "div" :: c :: v :: Nil =>
      Div(
        c.head,
        if (v.exists(_.isDigit)) Integer(v.toInt) else Variable(v.head)
      )
    case "mod" :: c :: v :: Nil =>
      Mod(
        c.head,
        if (v.exists(_.isDigit)) Integer(v.toInt) else Variable(v.head)
      )
    case "eql" :: c :: v :: Nil =>
      Eql(
        c.head,
        if (v.exists(_.isDigit)) Integer(v.toInt) else Variable(v.head)
      )
    case _ => sys.error(s"Bad instruction: $s")
  }

  class Algorithm(instructions: List[Instruction]) {
    val cache = mutable.Map.empty[String, Map[Char, Long]]
    val inputIndices = instructions.zipWithIndex.collect { case (_: Inp, i) =>
      i
    }

    def apply(l: Long): Unit = {
      val s = l.toString
      val (instructionIndex, startingState, dropFromS) =
        (0 until s.length)
          .map(i => s.substring(0, i + 1))
          .collectFirst {
            case k if cache.contains(k) =>
              (inputIndices.drop(k.length).head, cache(k), k.length)
          }
          .getOrElse((0, Map.empty[Char, Long], 0))
      val zEnd = instructions
        .drop(instructionIndex)
        .foldLeft((startingState, s.drop(dropFromS))) {
          case ((state, inputLeft), instruction) =>
            instruction match {
              case i: Inp =>
                cache.addOne(
                  s.substring(0, s.length - inputLeft.length) -> state
                )
                i(
                  state,
                  inputLeft.headOption.map(_.toString.toInt)
                ) -> inputLeft.tail
              case o: Operation =>
                try {
                  o(state, None) -> inputLeft
                } catch {
                  case _: Throwable =>
                    throw InstructionException(inputLeft.length)
                }
            }
        }
        ._1
        .getOrElse('z', 0)
      if (zEnd == 0) ()
      else throw InstructionException(0)
    }
  }

  val algo = new Algorithm(input.map(parseInstruction))

  case class InstructionException(inputLeft: Int) extends RuntimeException

  override def partOne(): String = {
    @tailrec
    def loop(l: Long, i: Int): String = {
      if (i % 10000 == 0) println(l)
      val s = l.toString
      s.zipWithIndex.find(_._1 == '0') match {
        case Some((_, index)) =>
          loop(l - Math.pow(10, (13 - index)).toInt, i + 1)
        case None =>
          Try(algo(l)) match {
            case Failure(InstructionException(inputLeft)) =>
              loop(l - Math.pow(10, inputLeft).toInt, i + 1)
            case Success(_) => s
            case Failure(t) => throw t
          }
      }
    }
    loop(99999999999999L, 1)
  }
  override def partTwo(): String = "???"
}
