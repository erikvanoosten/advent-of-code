package nl.grons.advent.y2022

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.nio.file.{Files, Path}
import scala.annotation.tailrec

/**
 * See [[https://adventofcode.com/2022/day/25]].
 */
object Day25 extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val useExampleInput = false

    val fileInput = Files.readString(Path.of("src/main/resources/y2022/input-day25.txt"))

    val exampleInput =
      """1=-0-2
        |12111
        |2=0=
        |21
        |2=01
        |111
        |20012
        |112
        |1=-1=
        |1-12
        |12
        |1=
        |122
        |""".stripMargin

    val input = if (useExampleInput) exampleInput else fileInput

    def snafuDigitValue(digit: Char): Int = "=-012".indexOf(digit) - 2
    def valueToSnafuDigit(value: Int): Char = "=-012"(value + 2)

    def parseSnafu(snafu: String): Long = {
      snafu
        .reverse
        .zipWithIndex
        .map { case (snafuDigit, position) =>
          Math.pow(5, position).toLong * snafuDigitValue(snafuDigit)
        }
        .sum
    }

    def formatAsSnafu(value: Long): String = {
      @tailrec
      def loop(value: Long, followingDigits: String): String = {
        if (value == 0) followingDigits
        else {
          val decDigitValue = Math.floorMod(value, 5)
          val toEncode = if (decDigitValue > 2) decDigitValue - 5 else decDigitValue
          val remaining = value - toEncode
          loop(remaining / 5, valueToSnafuDigit(toEncode) + followingDigits)
        }
      }
      loop(value, "")
    }

    val result = formatAsSnafu(
        input
          .linesIterator
          .takeWhile(_.nonEmpty)
          .map(parseSnafu)
          .sum
      )

    zio.Console.printLine("Result part 1: " + result)  // 121=2=1==0=10=2-20=2
  }

}
