package nl.grons.advent.y2022

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.nio.file.{Files, Path}
import scala.annotation.tailrec

/**
 * See [[https://adventofcode.com/2022/day/17]].
 */
object Day17 extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val input = Files.readString(Path.of("src/main/resources/y2022/input-day17.txt")).strip()

    val input1 = """>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"""

    val rocks: Seq[Seq[Int]] = Seq(
      Seq(
        "..####."
      ),
      Seq(
        "...#...",
        "..###..",
        "...#...",
      ),
      Seq(
        "....#..",
        "....#..",
        "..###..",
      ),
      Seq(
        "..#....",
        "..#....",
        "..#....",
        "..#....",
      ),
      Seq(
        "..##...",
        "..##...",
      ),
    ).map {
      _
        .map { s =>
          val binary = s.replace('.', '0').replace('#', '1')
          Integer.parseUnsignedInt(binary, 2)
        }
        .reverse
    }

    type Chamber = Map[Int, Int]
    val emptyChamber = Map.empty[Int, Int]

    def collision(chamber: Chamber, rock: Seq[Int], rockHeight: Int): Boolean = {
      rock
        .zip(Iterator.from(rockHeight))
        .exists { case (rockLine, height) =>
          chamber.get(height).exists(chamberLine => (chamberLine & rockLine) != 0)
        }
    }

    def jetPush(rock: Seq[Int], direction: Char): Seq[Int] = {
      val (canBePushed, pushed) = direction match {
        case '<' => rock.forall(_ >> 6 == 0) -> rock.map(_ << 1)
        case '>' => rock.forall(rl => (rl & 1) == 0) -> rock.map(_ >> 1)
      }
      if (canBePushed) pushed else rock
    }

    assert(jetPush(Seq(1), '>') == Seq(1))
    assert(jetPush(Seq(2), '>') == Seq(1))
    assert(jetPush(Seq(1), '<') == Seq(2))
    assert(jetPush(Seq(63), '<') == Seq(126))
    assert(jetPush(Seq(126), '<') == Seq(126))

    def addRockToChamber(chamber: Chamber, rock: Seq[Int], rockHeight: Int): Chamber = {
      rock
        .zip(Iterator.from(rockHeight))
        .foldLeft(chamber) { case (acc, (rockLine, height)) =>
          acc + (height -> (acc.getOrElse(height, 0) | rockLine))
        }
    }

    def chamberHeight(chamber: Chamber): Int =
      if (chamber.isEmpty) 0 else chamber.keys.max + 1

    def printChamber(chamber: Chamber): Unit = {
      if (chamber.isEmpty) println("Empty chamber")
      else {
        println("Chamber")
        (0 until chamberHeight(chamber)).reverse.foreach { lineNo =>
          val r = Integer.toString(chamber.getOrElse(lineNo, 0), 2).replace('0', '.').replace('1', '#')
          val pr = "." * (7 - r.length) + r
          println("%4d |%s|".format(lineNo, pr))
        }
        println("     +-------+")
      }
    }

    def dropRocks(initialChamber: Chamber, rockStream: Iterator[Seq[Int]], jets: Iterator[Char]): Chamber = {
      rockStream
        .foldLeft(initialChamber) { case (chamber, nextRock) =>
          @tailrec
          def pushAndDropLoop(rock: Seq[Int], rockHeight: Int): (Seq[Int], Int) = {
            // Push by jet
            val jetDirection = jets.next()
            val pushed = jetPush(rock, jetDirection)
            val afterPush = if (collision(chamber, pushed, rockHeight)) rock else pushed
            // Drop 1 height
            val canDrop = rockHeight > 0 && !collision(chamber, afterPush, rockHeight - 1)
            if (canDrop) pushAndDropLoop(afterPush, rockHeight - 1) else (afterPush, rockHeight)
          }

          val rockHeight = chamberHeight(chamber) + 3
          val (settledRock, settledRockHeight) = pushAndDropLoop(nextRock, rockHeight)
          addRockToChamber(chamber, settledRock, settledRockHeight)
        }
    }

    def rockStream(rockCount: Int): Iterator[Seq[Int]] = Iterator.continually(rocks).flatten.take(rockCount)
    def infiniteJetStream(): Iterator[Char] = Iterator.continually(input).flatten

    def dropRocks1(rockCount: Int): Int = {
      val chamber = dropRocks(emptyChamber, rockStream(rockCount), infiniteJetStream())
      chamberHeight(chamber)
    }

    // Part 2

    def dropRocks2(rockCount: Long): Long = {
      // Since:
      //  - rock shapes repeat after all 5 rock shapes have dropped
      //  - jets repeat after input.size pushes
      // repetition will occur at some moment a multiple of 5 rockets have dropped and
      // a multiple of input.size jets have pushed.
      //
      // Complications:
      //  - just above the floor the rock behave differently
      //  - we don't know after which multiple of 5 the repetition starts
      //
      // Solution:
      //  - first drop a bunch of rocks to lay a foundation
      //  - continue to drop 5 rocks, while keeping track of the jet count and chamber height
      //  - until we see a multiple of 5 rocks that used a multiple of input.size jet pushes AND
      //    has the same settled rocks twice

      val jets: CountingIterator[Char] = new CountingIterator(infiniteJetStream())

      // Lay a base
      val baseRockCount = 10 * rocks.size
      val baseChamber = dropRocks(emptyChamber, rockStream(baseRockCount), jets)
      val baseChamberJets = jets.iterCount()
      val baseChamberH = chamberHeight(baseChamber)

      // Explore base extensions
      case class BaseChamber(rockCount: Int, jetCount: Int, height: Int)
      var extendedBaseChambers: Vector[BaseChamber] =
        Vector(BaseChamber(baseRockCount, baseChamberJets, baseChamberH))

      // Find the number of rocks to drop for the assumed repetition
      var extensionChamber = baseChamber
      var extensionRockCount = baseRockCount
      var repetitionFound: Option[BaseChamber] = None
      do {
        extensionChamber = dropRocks(extensionChamber, rockStream(rocks.size), jets)
        extensionRockCount += rocks.size
        val extensionChamberHeight = chamberHeight(extensionChamber)
        val extensionJetCount = jets.iterCount()

        repetitionFound = extendedBaseChambers.find { extended =>
          ((extensionJetCount - extended.jetCount) % input.length == 0) && {
            val extensionDeltaHalfHeight = (extensionChamberHeight - extended.height) / 2
            val bottomPart = Range(extended.height - 20, extended.height + extensionDeltaHalfHeight - 20).map(extensionChamber)
            val topPart = Range(extended.height + extensionDeltaHalfHeight - 20, extensionChamberHeight - 20).map(extensionChamber)
            bottomPart == topPart
          }
        }
        extendedBaseChambers = extendedBaseChambers :+ BaseChamber(extensionRockCount, extensionJetCount, extensionChamberHeight)
      } while (repetitionFound.isEmpty)

      val repetitionBase = repetitionFound.get

      val beforeRepetitionRockCount = repetitionBase.rockCount
      val repetitionSizeInRocks = extensionRockCount - beforeRepetitionRockCount

      val repetitionSizeInJets = jets.iterCount() - repetitionBase.jetCount

      val beforeRepetitionChamberHeight = repetitionBase.height
      val afterRepetitionChamberHeight = chamberHeight(extensionChamber)
      val repetitionSizeInHeight = afterRepetitionChamberHeight - beforeRepetitionChamberHeight

      println(s"Found a repetition of $repetitionSizeInRocks rocks and $repetitionSizeInJets jets")
      println(s"Base chamber height is $beforeRepetitionChamberHeight")
      println(s"Repetition height is $repetitionSizeInHeight")

      val repeatCount: Long = (rockCount - beforeRepetitionRockCount) / repetitionSizeInRocks
      val projectedChamberH: Long = repeatCount * repetitionSizeInHeight

      val stillToDrop: Long = rockCount - beforeRepetitionRockCount - (repeatCount * repetitionSizeInRocks)
      assert(stillToDrop < Int.MaxValue)
      val afterChamber = dropRocks(extensionChamber, rockStream(stillToDrop.toInt), jets)
      val afterChamberHDelta = chamberHeight(afterChamber) - afterRepetitionChamberHeight

      assert(beforeRepetitionRockCount + repeatCount * repetitionSizeInRocks + stillToDrop == rockCount)

      beforeRepetitionChamberHeight + projectedChamberH + afterChamberHDelta
    }

    val resultPart1 = dropRocks1(2022)

    val resultPart2 = dropRocks2(1000000000000L)

    zio.Console.printLine("Result part 1: " + resultPart1) *> // 3206
      zio.Console.printLine("Result part 2: " + resultPart2) // 1602881844347
  }

  class CountingIterator[A](wrapped: Iterator[A]) extends Iterator[A] {
    private var cnt: Int = 0
    override def hasNext: Boolean = wrapped.hasNext
    override def next(): A = {
      cnt += 1
      wrapped.next()
    }
    def iterCount(): Int = cnt
  }

}
