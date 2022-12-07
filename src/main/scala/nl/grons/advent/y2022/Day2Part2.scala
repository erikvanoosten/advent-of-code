package nl.grons.advent.y2022

import zio.stream.ZPipeline.{splitLines, utfDecode}
import zio.stream.ZStream
import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.File

/**
 * See [[https://adventofcode.com/2022/day/2]].
 */
object Day2Part2 extends ZIOAppDefault {

  sealed abstract class RPS(val value: Int)
  object Rock extends RPS(1)
  object Paper extends RPS(2)
  object Scissors extends RPS(3)

  object RPS {
    def fromElf(elf: String): RPS = elf match {
      case "A" => Rock
      case "B" => Paper
      case "C" => Scissors
    }

    def scoreForSecond(first: RPS, second: RPS): Int = {
      val outcome = (first, second) match {
        case (Rock, Paper) => 6
        case (Rock, Scissors) => 0
        case (Paper, Rock) => 0
        case (Paper, Scissors) => 6
        case (Scissors, Rock) => 6
        case (Scissors, Paper) => 0
        case _ => 3
      }
      outcome + second.value
    }
  }

  sealed abstract class Outcome(val value: Int)
  object Win extends Outcome(6)
  object Draw extends Outcome(3)
  object Loose extends Outcome(0)

  object Outcome {
    def fromInput(elf: String): Outcome = elf match {
      case "X" => Loose
      case "Y" => Draw
      case "Z" => Win
    }

    def selectRpsForOutcome(elf1: RPS, outcomeForSecond: Outcome): RPS = outcomeForSecond match {
      case Win => elf1 match {
        case Paper => Scissors
        case Rock => Paper
        case Scissors => Rock
      }
      case Draw => elf1
      case Loose => elf1 match {
        case Scissors => Paper
        case Paper => Rock
        case Rock => Scissors
      }
    }
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    ZStream
      .fromFile(new File("src/main/resources/y2022/input-day2.txt"))
      .via(utfDecode >>> splitLines)
      .map(_.split(' '))
      .map { case Array(col1, col2) =>
        val rpsElf1 = RPS.fromElf(col1)
        val outcomeElf2 = Outcome.fromInput(col2)
        val rpsElf2 = Outcome.selectRpsForOutcome(rpsElf1, outcomeElf2)
        RPS.scoreForSecond(rpsElf1, rpsElf2)
      }
      .runSum
      .flatMap(score => zio.Console.printLine("Score: " + score)) // 13131
  }

}
