package nl.grons.advent.y2022

import zio.stream.ZPipeline.{splitLines, utfDecode}
import zio.stream.{ZSink, ZStream}
import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.File
import scala.collection.mutable

/**
 * See [[https://adventofcode.com/2022/day/5]].
 */
object Day5Part2 extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    ZStream
      .fromFile(new File("src/main/resources/y2022/input-day5.txt"))
      .via(utfDecode >>> splitLines)
      .run(ZSink.collectAll)
      .map { lines =>
        val stackCount = (lines(0).length + 1) / 4
        val (stackLines, moveLines) = lines.splitWhere(_.isEmpty)
        val stacks = (0 until stackCount).map { stackNo =>
          val stack = stackLines
            .dropRight(1)
            .flatMap { stackLine =>
              Option(stackLine(stackNo * 4 + 1)).filter(_ != ' ')
            }
          mutable.Stack.empty ++ stack
        }

        moveLines
          .filter(_.nonEmpty)  // empty line after the initial stacks
          .foreach { move =>
            // move 3 from 2 to 9
            val moveParts = move.split(' ')
            val moveCount = moveParts(1).toInt
            val fromStack = moveParts(3).toInt - 1
            val toStack = moveParts(5).toInt - 1
            val crates = (1 to moveCount).map(_ => stacks(fromStack).pop())
            crates.reverse.foreach(crate => stacks(toStack).push(crate))
          }

        stacks.map(_.top).mkString
      }
      .flatMap(result => zio.Console.printLine("Result: " + result)) // QRQFHFWCL
  }

}
