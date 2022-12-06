package nl.grons.advent.y2022

import zio.stream.ZPipeline.{splitLines, utfDecode}
import zio.stream.ZStream
import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.File
import scala.collection.immutable.BitSet

object Day4Part1 extends ZIOAppDefault {

  case class AreaIdRange(ids: Range) {
    def toSet: Set[Int] = BitSet.empty ++ ids
    def fullyContains(other: AreaIdRange): Boolean = other.toSet.subsetOf(toSet)
  }

  object AreaIdRange {
    def fromStr(rangeExpr: String): AreaIdRange = {
      val Array(begin, end) = rangeExpr.split('-')
      AreaIdRange(begin.toInt to end.toInt)
    }
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    ZStream
      .fromFile(new File("src/main/resources/y2022/input-day4.txt"))
      .via(utfDecode >>> splitLines)
      .map { line =>
        val Array(range1, range2) = line.split(',')
        val ids1 = AreaIdRange.fromStr(range1)
        val ids2 = AreaIdRange.fromStr(range2)
        val fullyContains = ids1.fullyContains(ids2) || ids2.fullyContains(ids1)
        if (fullyContains) 1 else 0
      }
      .runSum
      .flatMap(score => zio.Console.printLine("Score: " + score)) // 534
  }

}
