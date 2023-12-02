package nl.grons.advent.y2023

import java.nio.file.{Files, Path}
import fastparse._
import fastparse.SingleLineWhitespace._

object Day2 {

  final case class Draw(red: Int, green: Int, blue: Int) {
    def possible(maxDraw: Draw): Boolean = {
      red <= maxDraw.red && green <= maxDraw.green && blue <= maxDraw.blue
    }
  }

  object Draw {
    def fromMap(map: Map[String, Int]): Draw =
    Draw(map.getOrElse("red", 0), map.getOrElse("green", 0), map.getOrElse("blue", 0))
  }

  final case class Game(id: Int, draws: Seq[Draw]) {
    def possible(maxDraw: Draw): Boolean = draws.forall(_.possible(maxDraw))

    def power: Int = {
      val red = draws.map(_.red).max
      val green = draws.map(_.green).max
      val blue = draws.map(_.blue).max
      red * green * blue
    }
  }

  object Game {
    def intParser[_: P]: P[Int] = P(CharsWhileIn("1234567890").!).map(_.toInt)
    def cubeParser[_: P]: P[(String, Int)] = P(intParser ~ ("red" | "green" | "blue").!).map(_.swap)
    def drawParser[_: P]: P[Draw] = P(cubeParser.rep(sep = ",")).map(hands => Draw.fromMap(hands.toMap))
    def drawsParser[_: P]: P[Seq[Draw]] = P(drawParser.rep(sep = ";"))
    def gameParse[_: P]: P[Game] = P("Game" ~ intParser ~ ":" ~ drawsParser).map((Game.apply _).tupled) // .map { case (id, hands) => Game.apply(id, hands) }

    // "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    def fromLine(line: String): Game = {
      parse(line, gameParse(_)) match {
        case Parsed.Success(value, _) => value
        case _: Parsed.Failure => sys.error(s"Invalid game '$line'")
      }
    }
  }

  def main(args: Array[String]): Unit = {

    val input = Files.readString(Path.of("src/main/resources/y2023/day02.txt"))

    val maxDraw = Draw(red = 12, green = 13, blue = 14)

    val solution1 = input
      .linesIterator
      .map(Game.fromLine)
      .filter(_.possible(maxDraw))
      .map(_.id)
      .sum

    println("Solution 1: " + solution1)

    val solution2 = input
      .linesIterator
      .map(Game.fromLine)
      .map(_.power)
      .sum

    println("Solution 2: " + solution2)
  }
}
