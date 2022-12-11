package nl.grons.advent.y2022

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.nio.file.{Files, Path}

/**
 * See [[https://adventofcode.com/2022/day/9]].
 */
object Day9 extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val input = Files.readString(Path.of("src/main/resources/y2022/input-day9.txt"))

//    val input = """R 4
//                  |U 4
//                  |L 3
//                  |D 1
//                  |R 4
//                  |D 1
//                  |L 5
//                  |R 2
//                  |""".stripMargin

//    val input = """R 5
//                   |U 8
//                   |L 8
//                   |D 3
//                   |R 17
//                   |D 10
//                   |L 25
//                   |U 20""".stripMargin

    case class Position(x: Int, y: Int) {
      def up: Position = copy(y = y + 1)
      def down: Position = copy(y = y - 1)
      def right: Position = copy(x = x + 1)
      def left: Position = copy(x = x - 1)

      def isAdjacent(other: Position): Boolean = {
        (x - other.x).abs <= 1 && (y - other.y).abs <= 1
      }
      def follow(head: Position): Position = {
        if (isAdjacent(head)) this else {
          val xDirection = head.x - x
          val yDirection = head.y - y
          copy(x + xDirection.sign, y + yDirection.sign).follow(head)
        }
      }
    }

    assert(Position(0,0).isAdjacent(Position(0,0)))
    assert(Position(0,0).isAdjacent(Position(0,1)))
    assert(Position(0,0).isAdjacent(Position(1,0)))
    assert(Position(0,0).isAdjacent(Position(1,1)))
    assert(Position(0,0).isAdjacent(Position(-1,-1)))

    assert(Position(0,0).follow(Position(1,0)) == Position(0,0))
    assert(Position(0,0).follow(Position(1,1)) == Position(0,0))
    assert(Position(0,0).follow(Position(2,0)) == Position(1,0))
    assert(Position(0,0).follow(Position(2,0)) == Position(1,0))
    assert(Position(0,0).follow(Position(1,2)) == Position(1,1))
    assert(Position(0,0).follow(Position(2,1)) == Position(1,1))
    assert(Position(0,0).follow(Position(3,1)) == Position(2,1))

    val start = Position(0, 0)

    val tailVisits = input
      .linesIterator
      .foldLeft((start, start, Set(start))) { case (acc, line) =>
        val Array(direction, stepCount) = line.split(' ')
        (1 to stepCount.toInt).foldLeft(acc) { case ((head, tail, tailVisits), _) =>
          val headUpdate = direction match {
            case "R" => head.right
            case "U" => head.up
            case "L" => head.left
            case "D" => head.down
          }
          val tailUpdate = tail.follow(headUpdate)
          val tailVisitsUpdate = tailVisits + tailUpdate
          (headUpdate, tailUpdate, tailVisitsUpdate)
        }
      }
      ._3

    val resultPart1 = tailVisits.size

    // Part 2

    val knots = Seq.fill(10)(start)

    val tailVisitsPart2 = input
      .linesIterator
      .foldLeft((knots, Set(start))) { case (acc, line) =>
        val Array(direction, stepCount) = line.split(' ')
        (1 to stepCount.toInt).foldLeft(acc) { case ((knots, tailVisits), _) =>
          val headUpdate = direction match {
            case "R" => knots.head.right
            case "U" => knots.head.up
            case "L" => knots.head.left
            case "D" => knots.head.down
          }
          val knotsUpdate = knots.tail.foldLeft(Seq(headUpdate)) { case (acc, knot) =>
            acc :+ knot.follow(acc.last)
          }
          val tailVisitsUpdate = tailVisits + knotsUpdate.last
          (knotsUpdate, tailVisitsUpdate)
        }
      }
      ._2

    val resultPart2 = tailVisitsPart2.size

    zio.Console.printLine("Result part 1: " + resultPart1) *> // 6023
      zio.Console.printLine("Result part 2: " + resultPart2) // 2533
  }

}
