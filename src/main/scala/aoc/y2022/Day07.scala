package aoc
package y2022

import scala.collection.mutable
import java.util.concurrent.atomic.AtomicReference

object Day07 extends Day(7, 2022) {
  sealed trait SysObject {
    def name: String
    def size: Long
  }
  class Directory(val name: String) extends SysObject {
    var contents: List[SysObject] = Nil

    def setContents(ls: Ls): Unit = {
      contents = ls.contents
    }

    def getDirectory(n: String): Directory = {
      contents.find(_.name == n).getOrElse(sys.error(s"$n not found")) match {
        case d: Directory => d
        case _: File => sys.error(s"$n is a file")
      }
    }

    def du: String = {
      val buffer = new StringBuilder()
      def loop(toPrint: List[(SysObject, Int)]): String = toPrint match {
        case Nil => buffer.toString()
        case (obj, depth) :: tail =>
          val spaces = List.fill(depth*2)(' ').mkString
          buffer.append(s"$spaces- $obj\n")
          obj match {
            case d: Directory => loop(d.contents.sortBy(_.name).map(_ -> (depth + 1)) ++ tail)
            case _: File => loop(tail)
          }
      }
      loop((this, 0) :: Nil)
    }

    override def toString: String = s"$name (dir, size=$size)"

    def allDirectories: List[Directory] = this :: contents.collect { case d: Directory => d }.flatMap(d => d.allDirectories)
    override def size: Long = contents.map(_.size).sum
  }
  case class File(name: String, size: Long) extends SysObject {
    override def toString: String = s"$name (file, size=$size)"
  }

  sealed trait Command
  case class Ls(contents: List[SysObject]) extends Command
  case class Cd(to: String) extends Command

  object Command {
    def parse(input: List[String]): List[Command] = {
      input.foldLeft[List[Command]](Nil) {
        case (list, command) if command.startsWith("$ cd ") => Cd(command.drop(5)) :: list
        case (list, "$ ls") => Ls(Nil) :: list
        case (Ls(contents) :: tail, f) if f.startsWith("dir ") => Ls(new Directory(f.drop(4)) :: contents) :: tail
        case (Ls(contents) :: tail, f) if f.head.isDigit =>
          val split = f.split(" ").toList
          Ls(File(split.last, split.head.toLong) :: contents) :: tail
        case _ => sys.error("invalid input")
      }.reverse
    }
  }

  def buildDirectory(commands: List[Command]): Directory = {
    def loop(stack: List[Directory], commandsLeft: List[Command]): Directory = commandsLeft match {
      case Nil => stack.last
      case cur :: tail =>
        cur match {
          case Cd("..") => loop(stack.tail, tail)
          case Cd(next) =>
            if (stack.isEmpty) loop(new Directory(next) :: Nil, tail)
            else loop(stack.head.getDirectory(next) :: stack, tail)
          case ls: Ls =>
            stack.head.setContents(ls)
            loop(stack, tail)
        }
    }
    loop(Nil, commands)
  }

  val root = buildDirectory(Command.parse(input))

  override def partOne(): String = root.allDirectories.map(_.size).filter(_ <= 100000).sum.toString
  
  override def partTwo(): String = {
    val spaceNeeded = 30000000 - (70000000 - root.size)
    root.allDirectories.map(_.size).filter(_ >= spaceNeeded).min.toString
  }
  
}
