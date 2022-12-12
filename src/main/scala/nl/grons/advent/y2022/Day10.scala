package nl.grons.advent.y2022

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.nio.file.{Files, Path}
import scala.collection.immutable.BitSet

/**
 * See [[https://adventofcode.com/2022/day/10]].
 */
object Day10 extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val input = Files.readString(Path.of("src/main/resources/y2022/input-day10.txt"))

    case class CPU(cycle: Int, regX: Int) {
      def tick(increment: Int): CPU = copy(cycle = cycle + increment)
    }

    sealed trait Instruction {
      val duration: Int
      def execute(cpu: CPU): (Range, CPU)
      protected def activeCycles(cpu: CPU): Range =
        Range(cpu.cycle, cpu.cycle + duration)
    }
    case class Noop() extends Instruction {
      override val duration: Int = 1
      def execute(cpu: CPU): (Range, CPU) =
        activeCycles(cpu) -> cpu.tick(duration)
    }
    case class AddX(increment: Int) extends Instruction {
      override val duration: Int = 2
      def execute(cpu: CPU): (Range, CPU) =
        activeCycles(cpu) -> cpu.tick(duration).copy(regX = cpu.regX + increment)
    }
    object Instruction {
      def fromString(line: String): Instruction = {
        line.split(' ') match {
          case Array("noop") => Noop()
          case Array("addx", increment) => AddX(increment.toInt)
        }
      }
    }

    val totalSignalStrength = input
      .linesIterator
      .foldLeft((CPU(1, 1), 0)) { case ((cpu, signalStrengthAcc), line) =>
        val instruction = Instruction.fromString(line)
        val (cycles, cpuNext) = instruction.execute(cpu)
        val interestingCycle = cycles.find(_ % 40 == 20)
        val signalStrength = interestingCycle.map(_ * cpu.regX).getOrElse(0)
        val signalStrengthNext = signalStrengthAcc + signalStrength
        (cpuNext, signalStrengthNext)
      }
      ._2

    val resultPart1 = totalSignalStrength

    // Part 2

    case class Screen(pixels: Set[Int]) {
      def drawPixel(position: Int): Screen = copy(pixels + position)
      def show: String =
        (0 to 239)
          .map(pixel => if (pixels.contains(pixel)) "#" else ".")
          .grouped(40)
          .map(_.mkString)
          .mkString("\n")
    }

    val screen = input
      .linesIterator
      .foldLeft((CPU(1, 1), Screen(BitSet.empty))) { case ((cpu, screen), line) =>
        val instruction = Instruction.fromString(line)
        val (cycles, cpuNext) = instruction.execute(cpu)
        val screenNext = cycles.foldLeft(screen) { case (screen, cycle) =>
          val topRowSpritePositions = (cpu.regX - 1) to (cpu.regX + 1)
          val drawPosition = cycle - 1
          if (topRowSpritePositions.contains(drawPosition % 40)) screen.drawPixel(drawPosition)
          else screen
        }
        (cpuNext, screenNext)
      }
      ._2

    val resultPart2 = screen.show

    zio.Console.printLine("Result part 1: " + resultPart1) *> // 17020
      zio.Console.printLine("Result part 2:\n" + resultPart2)
    // ###..#....####.####.####.#.....##..####.
    // #..#.#....#.......#.#....#....#..#.#....
    // #..#.#....###....#..###..#....#....###..
    // ###..#....#.....#...#....#....#.##.#....
    // #.#..#....#....#....#....#....#..#.#....
    // #..#.####.####.####.#....####..###.####.
    // --> RLEZFLGE
  }

}
