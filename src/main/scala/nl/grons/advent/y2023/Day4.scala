package nl.grons.advent.y2023

import fastparse._
import fastparse.SingleLineWhitespace._

import java.nio.file.{Files, Path}

object Day4 {

  def main(args: Array[String]): Unit = {

     val input = Files.readString(Path.of("src/main/resources/y2023/day04.txt"))
//    val input =
//      """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
//        |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
//        |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
//        |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
//        |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
//        |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".stripMargin

    final case class ScratchCard(id: Int, winningNumbers: Set[Int], haveNumbers: Set[Int]) {
      def winningNumbersCount: Int = (winningNumbers intersect haveNumbers).size
      def points: Int = if (winningNumbersCount == 0) 0 else 1 << (winningNumbersCount - 1)
    }

    object ScratchCard {
      // "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
      def intParser[_: P]: P[Int] = P(CharsWhileIn("1234567890").!).map(_.toInt)
      def scratchCardParse[_: P]: P[ScratchCard] =
        P("Card" ~ intParser ~ ":" ~ intParser.rep.map(_.toSet) ~ "|" ~ intParser.rep.map(_.toSet))
          .map((ScratchCard.apply _).tupled)

      def fromLine(line: String): ScratchCard = {
        parse(line, scratchCardParse(_)) match {
          case Parsed.Success(value, _) => value
          case _: Parsed.Failure => sys.error(s"Invalid game '$line'")
        }
      }
    }

    val originalScratchCards = input
      .linesIterator
      .map(ScratchCard.fromLine)
      .toIndexedSeq

    val solution1 = originalScratchCards
      .map(_.points)
      .sum

    println("Solution 1: " + solution1) // 24733

    def followingCards(card: ScratchCard, n: Int): Seq[ScratchCard] =
      originalScratchCards
        .dropWhile(_ != card)
        .slice(1, n + 1)

    val cardCounts: Map[ScratchCard, Int] = originalScratchCards.map(_ -> 1).toMap
    val solution2 = originalScratchCards
      .foldLeft(cardCounts) { case (acc, card) =>
        val cardCount = acc(card)
        followingCards(card, card.winningNumbersCount)
          .foldLeft(acc) { case (acc, card) =>
            acc.updated(card, acc(card) + cardCount)
          }
      }
      .values
      .sum

    println("Solution 2: " + solution2) // 5422730
  }
}
