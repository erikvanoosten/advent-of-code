package nl.grons.advent.y2022

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.nio.file.{Files, Path}

/**
 * See [[https://adventofcode.com/2022/day/23]].
 */
object Day23 extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val useExampleInput = false

    val fileInput = Files.readString(Path.of("src/main/resources/y2022/input-day23.txt"))

    val exampleInput =
      """....#..
        |..###.#
        |#...#.#
        |.#...##
        |#.###..
        |##.#.##
        |.#..#..
        |""".stripMargin

    val input = if (useExampleInput) exampleInput else fileInput

    case class ElfPosition(row: Int, col: Int) {
      def allNorth: Seq[ElfPosition] = Seq(-1, 0, 1).map(d => ElfPosition(row - 1, col + d))
      def allSouth: Seq[ElfPosition] = Seq(-1, 0, 1).map(d => ElfPosition(row + 1, col + d))
      def allWest: Seq[ElfPosition] = Seq(-1, 0, 1).map(d => ElfPosition(row + d, col - 1))
      def allEast: Seq[ElfPosition] = Seq(-1, 0, 1).map(d => ElfPosition(row + d, col + 1))
      def allDirections: Seq[ElfPosition] = (allNorth ++ allSouth ++ allWest ++ allEast).distinct
      def north: ElfPosition = ElfPosition(row - 1, col)
      def south: ElfPosition = ElfPosition(row + 1, col)
      def west: ElfPosition = ElfPosition(row, col - 1)
      def east: ElfPosition = ElfPosition(row, col + 1)
    }

    val startPositions: Set[ElfPosition] =
      input
        .linesIterator
        .zipWithIndex
        .flatMap { case (line, row) =>
          line.zipWithIndex.filter(_._1 == '#').map {
            case (_, col) => ElfPosition(row, col)
          }
        }
        .toSet

    def square(positions: Set[ElfPosition]): (Int, Int, Int, Int) = positions
      .foldLeft((Int.MaxValue, Int.MinValue, Int.MaxValue, Int.MinValue)) {
        case ((minRow, maxRow, minCol, maxCol), elf) =>
          (Math.min(minRow, elf.row), Math.max(maxRow, elf.row), Math.min(minCol, elf.col), Math.max(maxCol, elf.col))
      }

    def printElves(positions: Set[ElfPosition]): Unit = {
      val (minRow, maxRow, minCol, maxCol) = square(positions)
      println(s"($minRow, $maxRow, $minCol, $maxCol)")
      (minRow to maxRow).foreach { row =>
        println((minCol to maxCol).map(col => if (positions.contains(ElfPosition(row, col))) '#' else '.').mkString)
      }
    }

//    printElves(startPositions)

    def allDirectionsClear(elf: ElfPosition, occupied: Set[ElfPosition]): Boolean =
      elf.allDirections.forall(p => !occupied.contains(p))

    case class Direction(id: String, lookAt: ElfPosition => Seq[ElfPosition], proposeMoveTo: ElfPosition => ElfPosition) {
      def proposeMove(elf: ElfPosition, occupied: Set[ElfPosition]): Option[ElfPosition] = {
        Option.when(lookAt(elf).forall(look => !occupied.contains(look))) {
          proposeMoveTo(elf)
        }
      }
    }
    val directions: Seq[Direction] = Seq(
      Direction("N", _.allNorth, _.north),
      Direction("S", _.allSouth, _.south),
      Direction("W", _.allWest, _.west),
      Direction("E", _.allEast, _.east)
    )
    def directionsCarousel: Iterator[Seq[Direction]] =
      Iterator.from(0).map { step =>
        val (a, b) = directions.splitAt(step % directions.size)
        b ++ a
      }

    def round(start: Set[ElfPosition], directions: Seq[Direction]): Set[ElfPosition] = {
      // first half: proposals
      val proposals: Set[(ElfPosition, ElfPosition)] = start
        .map { elf =>
          if (allDirectionsClear(elf, start)) {
            // Don't move
            elf -> elf
          } else {
            // If no direction applies, propose to stay at same location
            directions
              .flatMap(_.proposeMove(elf, start))
              .headOption
              .map(elf -> _)
              .getOrElse(elf -> elf)
          }
        }
      // second half: moves
      proposals
        .groupMap(_._2)(_._1)
        .flatMap { case (moveTo, elves) =>
          if (elves.size == 1) Seq(moveTo) // yes, move
          else elves // stay put
        }
        .toSet
    }

    val resultPart1 = {
      val endPositions = directionsCarousel
        .take(10)
        .foldLeft(startPositions) { case (positions, directions) =>
          round(positions, directions)
        }

      val (minRow, maxRow, minCol, maxCol) = square(endPositions)

      (for {
        row <- minRow to maxRow
        col <- minCol to maxCol
        if ! endPositions.contains(ElfPosition(row, col))
      } yield 1).size
    }

    // Part 2

    val resultPart2 = {
      directionsCarousel
        .scanLeft(startPositions) { case (positions, directions) =>
          round(positions, directions)
        }
        .sliding(2, 1)
        .takeWhile { case Seq(first, next) => first != next }
        .size + 1
    }

    zio.Console.printLine("Result part 1: " + resultPart1) *> // 4025
      zio.Console.printLine("Result part 2: " + resultPart2) // 935
  }

}
