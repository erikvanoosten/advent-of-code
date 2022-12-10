package nl.grons.advent.y2022

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.nio.file.{Files, Path}

/**
 * See [[https://adventofcode.com/2022/day/8]].
 */
object Day8 extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val fileContent = Files.readString(Path.of("src/main/resources/y2022/input-day8.txt"))

    val rows = fileContent.linesIterator.toSeq
    val columns = Seq.tabulate(rows.size)(c => rows.map(_(c)).mkString)

    def visibleTreesOnStartOfLine(line: String): Set[Int] = {
      line
        .zipWithIndex
        .foldLeft((0, Set.empty[Int])) { case ((heightSoFar, visible), (tree, position)) =>
          val height = tree.toInt
          if (height > heightSoFar) (height, visible + position)
          else (heightSoFar, visible)
        }
        ._2
    }

    def visibleTreesOnEndOfLine(line: String): Set[Int] =
      visibleTreesOnStartOfLine(line.reverse).map(line.length - 1 - _)

    def visibleOnLine(line: String): Set[Int] =
      visibleTreesOnStartOfLine(line) ++ visibleTreesOnEndOfLine(line)

    val horizontal = rows
      .zipWithIndex
      .flatMap { case (row, rowIndex) => visibleOnLine(row).map((_, rowIndex)) }
      .toSet
    val vertical = columns
      .zipWithIndex
      .flatMap { case (column, colIndex) => visibleOnLine(column).map((colIndex, _)) }
      .toSet
    val visibleTrees = horizontal ++ vertical

    val resultPart1 = visibleTrees.size

    // Part 2

    def scenicScore(row: Int, col: Int): Int = {
      val treeHeight = rows(row)(col).toInt
      def viewingDistance(trees: String): Int = {
        val vd = trees.takeWhile(_.toInt < treeHeight).length
        if (vd == trees.length) vd else vd + 1
      }

      val vdUp = viewingDistance(columns(col).take(row).reverse)
      val vdLeft = viewingDistance(rows(row).take(col).reverse)
      val vdDown = viewingDistance(columns(col).drop(row + 1))
      val vdRight = viewingDistance(rows(row).drop(col + 1))
      vdLeft * vdRight * vdUp * vdDown
    }

    val scores = for {
      row <- rows.indices
      col <- columns.indices
    } yield scenicScore(row, col)

    val resultPart2 = scores.max

    zio.Console.printLine("Result part 1: " + resultPart1) *> // 1779
      zio.Console.printLine("Result part 2: " + resultPart2) // 172224
  }

}
