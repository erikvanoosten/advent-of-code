package nl.grons.advent.y2022

import zio.stream.ZPipeline.{splitLines, utfDecode}
import zio.stream.ZStream
import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.File

object Day3Part1 extends ZIOAppDefault {

  def itemPriority(item: Char): Int = {
    if (item >= 'a' && item <= 'z') (item - 'a') + 1
    else (item - 'A') + 27
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    ZStream
      .fromFile(new File("src/main/resources/y2022/input-day3.txt"))
      .via(utfDecode >>> splitLines)
      .map(rucksack => rucksack.splitAt(rucksack.length / 2))
      .map { case (compartment1, compartment2) =>
        val common = compartment1.toSet & compartment2.toSet
        itemPriority(common.head)
      }
      .runSum
      .flatMap(score => zio.Console.printLine("Score: " + score)) // 8252
  }

}
