package nl.grons.advent.y2023

import java.nio.file.{Files, Path}

object Day3 {

  def main(args: Array[String]): Unit = {

     val input = Files.readString(Path.of("src/main/resources/y2023/day03.txt"))
//    val input =
//      """467..114..
//        |...*......
//        |..35..633.
//        |......#...
//        |617*......
//        |.....+.58.
//        |..592.....
//        |......755.
//        |...$.*....
//        |.664.598..""".stripMargin

    final case class PartNumber(value: Int, positions: Range) {
      def isNextPosition(position: Int): Boolean = positions.last + 1 == position
      def addDigit(digit: Char): PartNumber = {
        PartNumber(
          value * 10 + digit - '0',
          Range.inclusive(positions.start, positions.last + 1)
        )
      }
      def adjacentPositions: Set[Int] = Range.inclusive(positions.start - 1, positions.last + 1).toSet
      def adjacentToSymbolOnLine(line: EngineSchematicLine): Boolean = {
        line.symbolPositions.intersect(adjacentPositions).nonEmpty
      }
    }

    object PartNumber {
      def fromDigit(digit: Char, position: Int): PartNumber =
        PartNumber(digit - '0', Range.inclusive(position, position))
    }

    final case class EngineSchematicLine(partNumbers: Seq[PartNumber], symbolPositions: Set[Int])

    object EngineSchematicLine {
      lazy val empty: EngineSchematicLine = EngineSchematicLine(Seq.empty, Set.empty)

      def fromLine(line: String): EngineSchematicLine = {
        line
          .zipWithIndex
          .foldLeft(empty) { case (acc, (c, pos)) =>
            c match {
              case '.' => acc
              case d if d.isDigit && acc.partNumbers.lastOption.exists(_.isNextPosition(pos)) =>
                acc.copy(partNumbers = acc.partNumbers.init :+ acc.partNumbers.last.addDigit(d))
              case d if d.isDigit =>
                acc.copy(partNumbers = acc.partNumbers :+ PartNumber.fromDigit(c, pos))
              case _ =>
                // symbol
                acc.copy(symbolPositions = acc.symbolPositions + pos)
            }
          }
      }
    }

    val engineSchematic: Seq[EngineSchematicLine] = input
      .linesIterator
      .map(EngineSchematicLine.fromLine)
      .toIndexedSeq

    val solution1 = engineSchematic
      .zipWithIndex
      .flatMap { case (line, n) =>
        line.partNumbers.filter { lineNumber =>
          // on previous line
          engineSchematic.lift(n - 1).exists(lineNumber.adjacentToSymbolOnLine) ||
          // on current line
            lineNumber.adjacentToSymbolOnLine(line) ||
          // on next line
            engineSchematic.lift(n + 1).exists(lineNumber.adjacentToSymbolOnLine)
        }
      }
      .map(_.value)
      .sum

    println("Solution 1: " + solution1)

    def gearRatio(n: Int, pos: Int): Option[Int] = {
      val adjacentPartNumbers = engineSchematic
        .slice(n - 1, n - 1 + 3)
        .flatMap { line =>
          line.partNumbers.filter(_.adjacentPositions.contains(pos))
        }
      if (adjacentPartNumbers.size == 2) Some(adjacentPartNumbers.head.value * adjacentPartNumbers(1).value)
      else None
    }

    val solution2 = input
      .linesIterator
      .zipWithIndex
      .flatMap { case (line, n) =>
        line
          .zipWithIndex
          .flatMap {
            case ('*', pos) => gearRatio(n, pos)
            case _ => None
          }
      }
      .sum

    println("Solution 2: " + solution2)
  }
}
