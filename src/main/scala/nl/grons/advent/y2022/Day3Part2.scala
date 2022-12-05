package nl.grons.advent.y2022

import zio.stream.ZPipeline.{splitLines, utfDecode}
import zio.stream.ZStream
import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.File

object Day3Part2 extends ZIOAppDefault {

  def itemPriority(item: Char): Int = {
    if (item >= 'a' && item <= 'z') (item - 'a') + 1
    else (item - 'A') + 27
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    ZStream
      .fromFile(new File("src/main/resources/y2022/input-day3.txt"))
      .via(utfDecode >>> splitLines)
      .grouped(3)
      .map { elfGroup =>
        assert(elfGroup.size == 3)
        val badgeSet = elfGroup(0).toSet & elfGroup(1).toSet & elfGroup(2).toSet
        itemPriority(badgeSet.head)
      }
      .runSum
      .flatMap(score => zio.Console.printLine("Score: " + score)) // 2828
  }

}
