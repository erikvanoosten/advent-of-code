package nl.grons.advent.y2022

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.nio.file.{Files, Path}

/**
 * See [[https://adventofcode.com/2022/day/20]].
 *
 */
object Day20 extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val input = Files.readString(Path.of("src/main/resources/y2022/input-day20.txt"))

    val exampleInput =
      """1
        |2
        |-3
        |3
        |-2
        |0
        |4
        |""".stripMargin

    val values: Vector[BigInt] = input.linesIterator.map(BigInt.apply).toVector

    implicit class VectorMove[A](v: Vector[A]) {
      def move(itemIndex: Int, targetIndex: Int): Vector[A] = {
        if (itemIndex == targetIndex) {
          v
        } else if (itemIndex < targetIndex) {
          val (beforeItem, atItem) = v.splitAt(itemIndex)
          val afterItem = atItem.drop(1)
          val (beforeMove, afterMove) = afterItem.splitAt(targetIndex - itemIndex)
          beforeItem ++ beforeMove ++ Vector(v(itemIndex)) ++ afterMove
        } else {
          val (beforeItem, atItem) = v.splitAt(itemIndex)
          val afterItem = atItem.drop(1)
          val (beforeMove, afterMove) = beforeItem.splitAt(targetIndex)
          beforeMove ++ Vector(v(itemIndex)) ++ afterMove ++ afterItem
        }
      }
    }

    assert(Vector(0,1,2,3,4,5,6).move(1,5) == Vector(0,2,3,4,5,1,6))

    def mixing(values: Vector[BigInt], repeat: Int = 1): Vector[BigInt] = {
      val m = values.size - 1
      var result = values
      var indices = values.indices.toVector
      repeat.times {
        values.indices.foreach { i =>
          val from = indices.indexOf(i)
          val delta = result(from)
          val to =
            if (delta == 0) from
            else {
              val rawTo = from + delta
              val to = rawTo.mod(m).toInt
              if (to == 0) m else to
            }
          indices = indices.move(from, to)
          result = result.move(from, to)
        }
      }
      result
    }

    val resultPart1 = {
      val decrypted = mixing(values)
      val zeroIndex = decrypted.indexOf(0L)
      Seq(1000, 2000, 3000)
        .map(_ + zeroIndex)
        .map(i => Math.floorMod(i, decrypted.size))
        .map(decrypted)
        .sum
    }

    // Part 2

    val resultPart2 = {
      val data = values.map(l => l * BigInt(811589153L))
      val decrypted = mixing(data, 10)
      val zeroIndex = decrypted.indexOf(0)
      Seq(1000, 2000, 3000)
        .map(_ + zeroIndex)
        .map(i => Math.floorMod(i, decrypted.size))
        .map(decrypted)
        .sum
    }

    zio.Console.printLine("Result part 1: " + resultPart1) *> // 15297
      zio.Console.printLine("Result part 2: " + resultPart2) // 2897373276210
  }

}
