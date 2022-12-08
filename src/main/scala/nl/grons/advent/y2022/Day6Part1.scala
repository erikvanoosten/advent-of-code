package nl.grons.advent.y2022

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.nio.file.{Files, Path}

/**
 * See [[https://adventofcode.com/2022/day/6]].
 */
object Day6Part1 extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val packet = Files.readString(Path.of("src/main/resources/y2022/input-day6.txt"))
    val result = packet
      .sliding(4)
      .zipWithIndex
      .find { case (fourLetters, _) =>
        fourLetters.toSet.size == 4
      }
      .map(_._2 + 4)
      .getOrElse(Int.MaxValue)
    zio.Console.printLine("Result: " + result) // 1876
  }

}
