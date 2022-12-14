package nl.grons.advent.y2022

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.nio.file.{Files, Path}
import scala.collection.mutable

/**
 * See [[https://adventofcode.com/2022/day/12]].
 */
object Day12 extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val input = Files.readString(Path.of("src/main/resources/y2022/input-day12.txt"))

    val heights: Seq[Seq[Int]] =
      input
        .linesIterator
        .map { row =>
          row.toIndexedSeq.map {
            case 'S' => 0
            case 'E' => 26
            case h => h - 'a'
          }
        }
        .toIndexedSeq
    val xSize = heights.head.size
    val ySize = heights.size

    case class Position(x: Int, y: Int) {
      val height: Int = heights(y)(x)
      def neighbours: Seq[Position] = {
        val result = Seq.newBuilder[Position]
        if (x > 0) result += copy(x - 1, y)
        if (x < (xSize-1)) result += copy(x + 1, y)
        if (y > 0) result += copy(x, y - 1)
        if (y < (ySize-1)) result += copy(x, y + 1)
        result.result()
      }
      def possibleSteps: Seq[Position] =
        neighbours
          .filter(p => p.height <= height + 1)
    }

    // Bellman-Ford algorithm
    def findShortestPathLength(start: Position, end: Position): Int = {
      val shortestPathLengthByDestination: mutable.Map[Position, Int] =
        mutable.Map.empty.withDefault(_ => Int.MaxValue)
      shortestPathLengthByDestination.put(start, 1)
      val predecessors: mutable.Map[Position, Position] = mutable.Map.empty
      val visited: mutable.Set[Position] = mutable.Set.empty

      val inProgress: mutable.Queue[Position] = mutable.Queue.empty
      inProgress += start

      while (inProgress.nonEmpty) {
        val position = inProgress.dequeue()
        val shortestDistance = shortestPathLengthByDestination(position)
        position.possibleSteps.foreach { next =>
          if (!visited.contains(next)) {
            visited += next
            shortestPathLengthByDestination.put(next, shortestDistance + 1)
            predecessors.put(next, position)
            inProgress += next
          }
        }
      }

      shortestPathLengthByDestination(end) - 1
    }

    def findPosition(char: Char): Position = {
      val lines = input.linesIterator
      val Some((line, y)) = lines.zipWithIndex.find(_._1.contains(char))
      val x = line.indexOf(char)
      Position(x, y)
    }
    val end: Position = findPosition('E')

    val resultPart1 = {
      val start: Position = findPosition('S')

      findShortestPathLength(start, end)
    }

    // Part 2

    val resultPart2 = {
      // NOTE: calculating the distance many times is total overkill.
      // The Bellman-Ford algorithm already finds the distance to every
      // reachable position. So we could just start from the end position
      // and then get the path length to the positions with height 0.
      // However, since Position.possibleSteps is not symmetric, this would have
      // required more changes. Besides, the algorithm is fast enough.

      val starts = for {
        x <- 0 until xSize
        y <- 0 until ySize
        start = Position(x, y)
        if start.height == 0
      } yield start

      starts
        .map(start => findShortestPathLength(start, end))
        .min
    }

    zio.Console.printLine("Result part 1: " + resultPart1) *> // 423
      zio.Console.printLine("Result part 2: " + resultPart2) // 416
  }
}
