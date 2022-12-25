package nl.grons.advent.y2022

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.nio.file.{Files, Path}
import scala.collection.mutable

/**
 * See [[https://adventofcode.com/2022/day/24]].
 */
object Day24 extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val useExampleInput = false

    val fileInput = Files.readString(Path.of("src/main/resources/y2022/input-day24.txt"))

    val exampleInput =
      """#.######
        |#>>.<^<#
        |#.<..<<#
        |#>v.><>#
        |#<^v^^>#
        |######.#
        |""".stripMargin

    val input = if (useExampleInput) exampleInput else fileInput

    val lines = input.linesIterator.takeWhile(_.nonEmpty).toIndexedSeq
    val valleyWidth = lines.head.length - 2 // subtracting the left and right walls
    val valleyHeight = lines.size - 2 // subtracting the entry and exit walls

    case class Position(row: Int, col: Int) {
      def nextPositions: Seq[Position] = Seq.concat(
        Option.when(row > 1 && row < valleyHeight + 2 && col > 2)(copy(col = col - 1)), // left
        Option.when(row > 1 && row < valleyHeight + 2 && col < valleyWidth + 1)(copy(col = col + 1)), // right
        Option.when(row > 2 || this == BelowStart)(copy(row = row - 1)), // up
        Option.when(row < valleyHeight + 1 || this == AboveExit)(copy(row = row + 1)), // down
        Seq(this) // stay put
      )
      def blizzardLeft(distance: Int): Position = Position(row, Math.floorMod(col - 2 - distance, valleyWidth) + 2)
      def blizzardRight(distance: Int): Position = Position(row, Math.floorMod(col - 2 + distance, valleyWidth) + 2)
      def blizzardUp(distance: Int): Position = Position(Math.floorMod(row - 2 - distance, valleyHeight) + 2, col)
      def blizzardDown(distance: Int): Position = Position(Math.floorMod(row - 2 + distance, valleyHeight) + 2, col)
    }

    lazy val Start: Position = Position(1, lines.head.indexOf('.') + 1)
    lazy val Exit: Position = Position(lines.size, lines.last.indexOf('.') + 1)
    lazy val BelowStart: Position = Position(Start.row + 1, Start.col)
    lazy val AboveExit: Position = Position(Exit.row - 1, Exit.col)

    def atTime0(position: Position): Char = lines(position.row - 1)(position.col - 1)

    def canMoveTo(position: Position, atTime: Int): Boolean = {
      // start and exit are safe
      position == Start || position == Exit || (
        // Move the potential blizzard back in time and see if it exists
        atTime0(position.blizzardRight(-atTime)) != '>' &&
        atTime0(position.blizzardLeft(-atTime)) != '<' &&
        atTime0(position.blizzardUp(-atTime)) != '^' &&
        atTime0(position.blizzardDown(-atTime)) != 'v'
      )
    }

    case class TimedPosition(pos: Position, time: Int)

    // Bellman-Ford algorithm
    def travelTimeTo(start: TimedPosition, exit: Position): Int = {
      // In this method: distance == time
      val distanceByDestination: mutable.Map[TimedPosition, Int] =
        mutable.Map.empty.withDefault(_ => Int.MaxValue)
      distanceByDestination.put(start, 0)
      val predecessors: mutable.Map[TimedPosition, TimedPosition] = mutable.Map.empty
      val visited: mutable.Set[TimedPosition] = mutable.Set.empty
      visited += start

      val inProgress: mutable.Queue[TimedPosition] = mutable.Queue.empty
      inProgress += start

      while (!visited.exists(_.pos == exit) && inProgress.nonEmpty) {
        val timedPosition = inProgress.dequeue()
        val shortestDistance = distanceByDestination(timedPosition)
        timedPosition
          .pos
          .nextPositions
          .map(p => TimedPosition(p, timedPosition.time + 1))
          .filter(p => canMoveTo(p.pos, p.time))
          .foreach { next =>
            // don't go to the same time position, but also never go to the same position more then 4 times
            if (!visited.contains(next) && visited.count(_.pos == next.pos) < 5) {
              visited += next
              distanceByDestination.put(next, shortestDistance + 1)
              predecessors.put(next, timedPosition)
              inProgress += next
            }
          }
      }

      distanceByDestination
        .find(_._1.pos == exit)
        .map(_._2)
        .getOrElse(sys.error(s"$exit not reachable"))
    }

    val resultPart1 = {
      travelTimeTo(TimedPosition(Start, 0), Exit)
    }

    // Part 2

    val resultPart2 = {
      val back = travelTimeTo(TimedPosition(Exit, resultPart1), Start)
      val again = travelTimeTo(TimedPosition(Start, resultPart1 + back), Exit)
      resultPart1 + back + again
    }

    zio.Console.printLine("Result part 1: " + resultPart1) *> // 260
      zio.Console.printLine("Result part 2: " + resultPart2) // 747
  }

}
