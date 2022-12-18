package nl.grons.advent.y2022

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.nio.file.{Files, Path}

/**
 * See [[https://adventofcode.com/2022/day/14]].
 */
object Day14 extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val input = Files.readString(Path.of("src/main/resources/y2022/input-day14.txt"))

    val input2 = """498,4 -> 498,6 -> 496,6
                  |503,4 -> 502,4 -> 502,9 -> 494,9
                  |""".stripMargin

    case class Position(x: Int, y: Int) {
      def sandFallOptions: Seq[Position] = Seq(
        copy(y = y + 1), // down
        copy(x = x - 1, y = y + 1), // down-left
        copy(x = x + 1, y = y + 1)  // down-right
      )
    }

    val rockPaths: Seq[Seq[Position]] = input
      .linesIterator
      .map { line =>
        line
          .split("->")
          .map { point =>
            val Array(x, y) = point.split(',')
            Position(x.trim.toInt, y.trim.toInt)
          }
          .toIndexedSeq
      }
      .toIndexedSeq

    def fullPath(pos1: Position, pos2: Position): Seq[Position] = {
      val directionX = (pos2.x - pos1.x).sign
      val directionY = (pos2.y - pos1.y).sign
      val positions = Iterator
        .iterate(pos1)(p => Position(p.x + directionX, p.y + directionY))
        .takeWhile(_ != pos2) ++ Iterator(pos2)
      positions.toIndexedSeq
    }

    val soil: Map[Position, Char] = rockPaths
      .foldLeft(Map.empty[Position, Char]) { case (soil, rockPath) =>
        rockPath
          .zip(rockPath.tail)
          .foldLeft(soil) { case (soil, (pos1, pos2)) =>
            soil ++ fullPath(pos1, pos2).map(_ -> '#')
          }
      }
    val maxRockDepth: Int = soil.keys.map(_.y).max

    val resultPart1 = {
      def sandDropsTo(position: Position, occupied: Position => Boolean): Option[Position] = {
        def next(position: Position): Option[Position] =
          position.sandFallOptions.find(p => !occupied(p))
        Seq
          .unfold(position) { p =>
            next(p).filter(_.y <= maxRockDepth).map(n => (n,n))
          }
          .lastOption
          .filter(_.y < maxRockDepth)
      }

      def dropSand(soil: Map[Position, Char]): Option[Map[Position, Char]] = {
        val sandPosition = Position(500, 0)
        sandDropsTo(sandPosition, soil.contains).map { sandPos => soil + ((sandPos, 'o')) }
      }

      val drops = Iterator.unfold(soil) { soil => dropSand(soil).map(s => (s,s)) }
      drops.size
    }

    // Part 2

    val resultPart2 = {
      def sandDropsTo(position: Position, occupied: Position => Boolean): Option[Position] = {
        if (occupied(position)) None
        else {
          def next(position: Position): Option[Position] =
            position.sandFallOptions.find(p => !occupied(p))

          Seq
            .unfold(position) { p =>
              next(p).map(n => (n, n))
            }
            .lastOption
        }
      }

      val floor: Position => Boolean = p => p.y == (maxRockDepth + 2)

      def dropSand(soil: Map[Position, Char]): Option[Map[Position, Char]] = {
        val sandPosition = Position(500, 0)
        val occupied: Position => Boolean = p => soil.contains(p) || floor(p)
        sandDropsTo(sandPosition, occupied).map { sandPos => soil + ((sandPos, 'o')) }
      }

      val drops = Iterator.unfold(soil) { soil => dropSand(soil).map(s => (s, s)) }
      drops.size + 1
    }

    zio.Console.printLine("Result part 1: " + resultPart1) *> // 715
      zio.Console.printLine("Result part 2: " + resultPart2) // 25248
  }
}
