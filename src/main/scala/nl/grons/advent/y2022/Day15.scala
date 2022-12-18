package nl.grons.advent.y2022

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.nio.file.{Files, Path}

/**
 * See [[https://adventofcode.com/2022/day/15]].
 */
object Day15 extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
//    val part1Y = 10
    val part1Y = 2000000

//    val part2MaxCoordinate = 20
     val part2MaxCoordinate = 4000000

    val input = Files.readString(Path.of("src/main/resources/y2022/input-day15.txt"))

    val input1 = """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
                  |Sensor at x=9, y=16: closest beacon is at x=10, y=16
                  |Sensor at x=13, y=2: closest beacon is at x=15, y=3
                  |Sensor at x=12, y=14: closest beacon is at x=10, y=16
                  |Sensor at x=10, y=20: closest beacon is at x=10, y=16
                  |Sensor at x=14, y=17: closest beacon is at x=10, y=16
                  |Sensor at x=8, y=7: closest beacon is at x=2, y=10
                  |Sensor at x=2, y=0: closest beacon is at x=2, y=10
                  |Sensor at x=0, y=11: closest beacon is at x=2, y=10
                  |Sensor at x=20, y=14: closest beacon is at x=25, y=17
                  |Sensor at x=17, y=20: closest beacon is at x=21, y=22
                  |Sensor at x=16, y=7: closest beacon is at x=15, y=3
                  |Sensor at x=14, y=3: closest beacon is at x=15, y=3
                  |Sensor at x=20, y=1: closest beacon is at x=15, y=3
                  |""".stripMargin

    case class Position(x: Int, y: Int) {
      def distanceTo(other: Position): Int = (x - other.x).abs + (y - other.y).abs
      def tuningFrequency: Long = x * 4000000L + y
    }
    case class Sensor(sensorPosition: Position, nearestBeacon: Position) {
      val distanceToBeacon: Int = sensorPosition distanceTo nearestBeacon
      def coversPosition(p: Position): Boolean = (sensorPosition distanceTo p) <= distanceToBeacon
    }

    val SensorRE = raw"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)".r
    val sensors = input
      .linesIterator
      .map { case SensorRE(sx, sy, bx, by) => Sensor(Position(sx.toInt, sy.toInt), Position(bx.toInt, by.toInt)) }
      .toIndexedSeq

    val resultPart1 = {
      val xMin = sensors.map(s => s.sensorPosition.x - s.distanceToBeacon).min
      val xMax = sensors.map(s => s.sensorPosition.x + s.distanceToBeacon).max

      (xMin to xMax)
        .map(x => Position(x, part1Y))
        .filterNot(p => sensors.exists(_.nearestBeacon == p))
        .count(p => sensors.exists(_.coversPosition(p)))
    }

    // Part 2

    case class InclusiveRange(start: Int, end: Int) {
      def isEmpty: Boolean = !nonEmpty
      def nonEmpty: Boolean = start <= end
      def size: Int = if (nonEmpty) end - start + 1 else 0
      def -(other: InclusiveRange): Seq[InclusiveRange] = {
        if (end < other.start) Seq(this)
        else if (start > other.end) Seq(this)
        else Seq(InclusiveRange(start, other.start - 1), InclusiveRange(other.end + 1, end)).filter(_.nonEmpty)
      }
    }
    object InclusiveRange {
      def apply(r: Range): InclusiveRange = InclusiveRange(r.head, r.last)
    }

    assert(InclusiveRange(0 to 3) - InclusiveRange(4 to 7) == Seq(InclusiveRange(0 to 3)))
    assert(InclusiveRange(4 to 7) - InclusiveRange(0 to 3) == Seq(InclusiveRange(4 to 7)))
    assert(InclusiveRange(3 to 6) - InclusiveRange(0 to 9) == Seq.empty)
    assert(InclusiveRange(0 to 9) - InclusiveRange(3 to 6) == Seq(InclusiveRange(0 to 2), InclusiveRange(7 to 9)))
    assert(InclusiveRange(0 to 9) - InclusiveRange(0 to 3) == Seq(InclusiveRange(4 to 9)))
    assert(InclusiveRange(0 to 9) - InclusiveRange(6 to 9) == Seq(InclusiveRange(0 to 5)))

    case class MultiRange(ranges: Seq[InclusiveRange]) {
      def size: Int = ranges.map(_.size).sum
      def nonEmpty: Boolean = size > 0
      def head: Int = ranges.head.start
      def -(other: MultiRange): MultiRange = {
        MultiRange(
          other
            .ranges
            .foldLeft(ranges) { case (acc, otherRange) =>
              acc.flatMap(_ - otherRange)
            }
            .filter(_.nonEmpty)
        )
      }
    }
    object MultiRange {
      def apply(range: Range): MultiRange = MultiRange(Seq(InclusiveRange(range)))
    }

    assert(
      MultiRange(0 to 10) - MultiRange(Seq(InclusiveRange(-1 to 3), InclusiveRange(2 to 4), InclusiveRange(8 to 9))) ==
        MultiRange(Seq(InclusiveRange(5 to 7), InclusiveRange(10 to 10)))
    )

    implicit class SensorExtra(val sensor: Sensor) {
      def coversRangeForY(y: Int): Seq[InclusiveRange] = {
        val xSpread = sensor.distanceToBeacon - (sensor.sensorPosition.y - y).abs
        if (xSpread < 1) Seq.empty
        else Seq(InclusiveRange(sensor.sensorPosition.x - xSpread, sensor.sensorPosition.x + xSpread))
      }
    }

    val resultPart2 = {
      val yRange = 0 to part2MaxCoordinate
      val xRange = MultiRange(0 to part2MaxCoordinate)

      yRange
        .flatMap { y =>
          val covered = MultiRange(sensors.flatMap(s => s.coversRangeForY(y)))
          val possibleCoordinates = xRange - covered
          Option
            .when(possibleCoordinates.nonEmpty)(possibleCoordinates.head)
            .map(x => Position(x, y))
        }
        .head
        .tuningFrequency
    }

    zio.Console.printLine("Result part 1: " + resultPart1) *> // 4724228
      zio.Console.printLine("Result part 2: " + resultPart2) // 13622251246513
  }
}
