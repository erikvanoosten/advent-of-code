package nl.grons.advent.y2022

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.nio.file.{Files, Path}
import scala.annotation.tailrec

/**
 * See [[https://adventofcode.com/2022/day/17]].
 *
 * WARNING: solves part 2 only for the example input, not for the actual input.
 */
object Day17 extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val input1 = Files.readString(Path.of("src/main/resources/y2022/input-day17.txt")).strip()

    val input = """>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"""

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
      // !!!!!! WARNING !!!!!!
      //
      // Part 2 works fine for the example input.
      // Unfortunately, it does not find any repetition for the actual input.
      //
      // Apparently the replication does not occur on the investigated boundaries.
      //
      // Suspect is that after using all jets, we have not dropped an exact number
      // of rocks (and definitely not a plural of 5). The same is true after
      // using 5 * number of jets.
      //
      // END OF WARNING

      // Repetition can only occur after we have dropped n * rocks.size rocks.
      //
      // We'll first drop a bunch of rucks to lay a foundation.
      // Then we'll drop all rock shapes until we see repetition.

      val jets: CountingIterator[Char] = new CountingIterator(infiniteJetStream())

      // Lay a base
      val baseRockCount = 20 * rocks.size
      val baseChamber = dropRocks(emptyChamber, rockStream(baseRockCount), jets)
      val baseChamberH = chamberHeight(baseChamber)
      val baseChamberJets = jets.iterCount()

      // Find the number of rocks to drop for the assumed repetition
      var repetitionChamber = baseChamber
      var repetitionRockCount = 0
      var repetitionFound = false
      do {
        repetitionChamber = dropRocks(repetitionChamber, rockStream(rocks.size), jets)
        repetitionRockCount += rocks.size
        val repetitionChamberH = chamberHeight(repetitionChamber)
        val repetitionChamberHDelta = repetitionChamberH - baseChamberH
        val repetitionChamberPart1 = Range(baseChamberH - 20, baseChamberH + repetitionChamberHDelta / 2 - 20).map(repetitionChamber)
        val repetitionChamberPart2 = Range(baseChamberH + repetitionChamberHDelta / 2 - 20, repetitionChamberH - 20).map(repetitionChamber)
        repetitionFound = repetitionChamberPart1 == repetitionChamberPart2
        if (repetitionRockCount % 1000 == 0) println(s"at $repetitionRockCount rocks")
      } while (!repetitionFound)
      val repetitionChamberH = chamberHeight(repetitionChamber)
      val repetitionChamberHDelta = repetitionChamberH - baseChamberH

      println(s"Repetition after $repetitionRockCount rocks and ${jets.iterCount() - baseChamberJets} jets")

      val repeatCount: Long = (rockCount - baseRockCount) / repetitionRockCount
      val projectedChamberH: Long = repeatCount * repetitionChamberHDelta

      val stillToDrop: Long = rockCount - baseRockCount - (repeatCount * repetitionRockCount)
      assert(stillToDrop < Int.MaxValue)
      val afterChamber = dropRocks(repetitionChamber, rockStream(stillToDrop.toInt), jets)
      val afterChamberHDelta = chamberHeight(afterChamber) - repetitionChamberH

      assert(baseRockCount + repeatCount * repetitionRockCount + stillToDrop == rockCount)

      baseChamberH + projectedChamberH + afterChamberHDelta
    }

    val resultPart1 = dropRocks1(2022)

    val resultPart2 = dropRocks2(1000000000000L)

    zio.Console.printLine("Result part 1: " + resultPart1) *> // 3206
      zio.Console.printLine("Result part 2: " + resultPart2) //
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
