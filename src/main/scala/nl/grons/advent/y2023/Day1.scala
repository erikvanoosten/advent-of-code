package nl.grons.advent.y2023

import java.nio.file.{Files, Path}

object Day1 {

  def main(args: Array[String]): Unit = {

    val input = Files.readString(Path.of("src/main/resources/y2023/day01.txt"))

    val solution1 = input
      .linesIterator
      .map { line =>
        val first = line.find(_.isDigit).get - '0'
        val last = line.findLast(_.isDigit).get - '0'
        first * 10 + last
      }
      .sum

    println("Solution 1: " + solution1)

    val digits: Seq[(String, Int)] =
      (0 to 9).map(n => n.toString -> n) ++
      Array("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
        .zipWithIndex
        .map { case (s, i) => (s, i + 1) }

    val solution2 = input
      .linesIterator
      .zipWithIndex
      .map { case (line, n) =>
        val first = digits.minBy(d => Math.floorMod(line.indexOf(d._1), Int.MaxValue))._2
        val last = digits.maxBy(d => line.lastIndexOf(d._1))._2
        first * 10 + last
      }
      .sum

    println("Solution 2: " + solution2)
  }
}
