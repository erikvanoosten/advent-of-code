package nl.grons.advent.y2022

import zio.stream.ZPipeline.{splitLines, utfDecode}
import zio.stream.ZStream
import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.File

/**
 * See [[https://adventofcode.com/2022/day/2]].
 */
object Day2Part1 extends ZIOAppDefault {

  abstract class RPS(val value: Int)
  object Rock extends RPS(1)
  object Paper extends RPS(2)
  object Scissors extends RPS(3)

  object RPS {
    def fromElf(elf: String): RPS = elf match {
      case "A" => Rock
      case "B" => Paper
      case "C" => Scissors
      case "X" => Rock
      case "Y" => Paper
      case "Z" => Scissors
    }

    def scoreForSecond(first: RPS, second: RPS): Int = {
      val outcome = (first, second) match {
        case (f, s) if f == s => 3
        case (Rock, Paper) => 6
        case (Rock, Scissors) => 0
        case (Paper, Rock) => 0
        case (Paper, Scissors) => 6
        case (Scissors, Rock) => 6
        case (Scissors, Paper) => 0
      }
      outcome + second.value
    }
  }


  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    ZStream
      .fromFile(new File("src/main/resources/y2022/input-day2.txt"))
      .via(utfDecode >>> splitLines)
      .map(_.split(' '))
      .map { case Array(elf1, elf2) =>
        RPS.scoreForSecond(RPS.fromElf(elf1), RPS.fromElf(elf2))
      }
      .runSum
      .flatMap(score => zio.Console.printLine("Score: " + score)) // 13221
  }

}
