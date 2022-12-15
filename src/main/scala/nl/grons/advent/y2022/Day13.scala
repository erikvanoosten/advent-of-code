package nl.grons.advent.y2022

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.nio.file.{Files, Path}
import fastparse._
import fastparse.NoWhitespace._

import scala.annotation.tailrec
import scala.math.Ordering.Implicits._

/**
 * See [[https://adventofcode.com/2022/day/13]].
 */
object Day13 extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val input = Files.readString(Path.of("src/main/resources/y2022/input-day13.txt"))

    sealed trait PacketItem {
      def toListItem: ListItem
    }
    case class IntItem(value: Int) extends PacketItem {
      def toListItem: ListItem = ListItem(Seq(this))
    }
    case class ListItem(values: Seq[PacketItem]) extends PacketItem {
      def toListItem: ListItem = this
    }

    object PacketItem {
      def listParser[_: P]: P[ListItem] = P("[" ~ itemParser.rep(sep = ",") ~ "]").map(ListItem)
      def itemParser[_: P]: P[PacketItem] = P(listParser | intParser)
      def intParser[_: P]: P[IntItem] = P(CharsWhileIn("1234567890").!).map(v => IntItem(v.toInt))

      def apply(line: String): PacketItem = {
        parse(line, listParser(_)) match {
          case Parsed.Success(value, _) => value
          case _: Parsed.Failure => sys.error(s"Invalid packet '$line'")
        }
      }
    }
    implicit object PacketItemOrdering extends Ordering[PacketItem] {
      def compareListItems(ls: Seq[PacketItem], rs: Seq[PacketItem]): Int = {
        ls
          .zipAll(rs, null, null)
          .iterator
          .map {
            case (l, _) if l == null => -1
            case (_, r) if r == null => 1
            case (l, r) => PacketItemOrdering.compare(l, r)
          }
          .find(_ != 0)
          .getOrElse(0)
      }

      @tailrec
      def compare(left: PacketItem, right: PacketItem): Int = {
        (left, right) match {
          case (IntItem(l), IntItem(r)) => l.compare(r)
          case (ListItem(l), ListItem(r)) => compareListItems(l, r)
          case (l, r) => PacketItemOrdering.compare(l.toListItem, r.toListItem)
        }
      }
    }

    val resultPart1 = {
      input
        .linesIterator
        .sliding(2, 3)
        .zipWithIndex
        .map { case (group, i) =>
          val packetIndex = i + 1
          (packetIndex, PacketItem(group.head), PacketItem(group(1)))
        }
        .map { case (index, leftPacket, rightPacket) =>
          val inOrder = leftPacket < rightPacket
          if (inOrder) index else 0
        }
        .sum
    }

    // Part 2

    val resultPart2 = {
      val dividerPacket2 = PacketItem("[[2]]")
      val dividerPacket6 = PacketItem("[[6]]")

      val packets = Seq(dividerPacket2, dividerPacket6) ++
        input.linesIterator.filter(_.nonEmpty).map(PacketItem.apply)

      packets
        .sorted
        .zipWithIndex
        .map { case (packet, i) => (i + 1, packet) }
        .filter { case (_, packet) => packet == dividerPacket2 || packet == dividerPacket6 }
        .map(_._1)
        .product
    }

    zio.Console.printLine("Result part 1: " + resultPart1) *> // 6568
      zio.Console.printLine("Result part 2: " + resultPart2) // 19493
  }
}
