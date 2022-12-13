package nl.grons.advent.y2022

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.nio.file.{Files, Path}
import scala.collection.mutable

/**
 * See [[https://adventofcode.com/2022/day/11]].
 */
object Day11 extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val input = Files.readString(Path.of("src/main/resources/y2022/input-day11.txt"))

    case class Operation(lhs: String, op: String, rhs: String) {
      assert(lhs.matches("(old|[0-9]+)") && op.matches("[+*]") && rhs.matches("(old|[0-9]+)"))

      def apply(input: BigInt): BigInt = {
        def value(arg: String) = arg match {
          case "old" => input
          case intLiteral => BigInt(intLiteral)
        }

        val lhsValue = value(lhs)
        val rhsValue = value(rhs)
        op match {
          case "+" => lhsValue + rhsValue
          case "*" => lhsValue * rhsValue
        }
      }
    }
    case class Test(divisor: BigInt, trueMonkey: Int, falseMonkey: Int) {
      def targetMonkey(worryLevel: BigInt): Int = if (worryLevel % divisor == 0) trueMonkey else falseMonkey
    }
    case class Monkey(id: Int, items: Seq[BigInt], operation: Operation, test: Test, inspectCount: BigInt) {
      def addItem(worryLevel: BigInt): Monkey = copy(items = items :+ worryLevel)
      def doneInspecting: Monkey = copy(items = Seq.empty, inspectCount = inspectCount + items.size)
      def keepWorryLevelsManageable(factor: BigInt): Monkey = copy(items = items.map(_ % factor))
    }

    object Operation {
      def fromString(opStr: String): Operation = {
        val Array(lhs, op, rhs) = opStr.split(' ')
        Operation(lhs, op, rhs)
      }
    }
    object Monkey {
      def fromString(lines: Seq[String]): Monkey = {
        assert(lines.size == 6)
        val id = lines.head.drop(7).dropRight(1).toInt
        val items = lines(1).drop(18).split(',').map(s => BigInt(s.trim))
        val operationStr = lines(2).drop(19)
        val divisor = lines(3).drop(21).toInt
        val trueMonkey = lines(4).drop(29).toInt
        val falseMonkey = lines(5).drop(30).toInt
        Monkey(id, items, Operation.fromString(operationStr), Test(divisor, trueMonkey, falseMonkey), 0)
      }
    }

    val inputMonkeys: Seq[Monkey] = input
      .linesIterator
      .sliding(6, 7)
      .map(Monkey.fromString)
      .toIndexedSeq

    val resultPart1 = {
      val monkeys: mutable.Seq[Monkey] = inputMonkeys.to(mutable.Seq)

      def turn(monkeyId: Int): Unit = {
        val monkey = monkeys(monkeyId)
        monkey.items.foreach { item =>
          val worryLevel = monkey.operation(item) / 3
          val targetMonkeyId = monkey.test.targetMonkey(worryLevel)
          assert(targetMonkeyId != monkeyId)
          val targetMonkey = monkeys(targetMonkeyId)
          monkeys.update(targetMonkeyId, targetMonkey.addItem(worryLevel))
        }
        monkeys.update(monkeyId, monkey.doneInspecting)
      }

      def round(): Unit = {
        monkeys.indices.foreach(turn)
      }

      20.times(round())

      val levelOfMonkeyBusiness =
        monkeys
          .map(_.inspectCount)
          .sorted
          .takeRight(2)
          .product

      levelOfMonkeyBusiness
    }

    // Part 2

    val resultPart2 = {
      val monkeys: mutable.Seq[Monkey] = inputMonkeys.to(mutable.Seq)

      val factor = monkeys.map(_.test.divisor).product

      def turn(monkeyId: Int): Unit = {
        val monkey = monkeys(monkeyId)
        monkey.items.foreach { item =>
          val worryLevel = monkey.operation(item)
          val targetMonkeyId = monkey.test.targetMonkey(worryLevel)
          assert(targetMonkeyId != monkeyId)
          val targetMonkey = monkeys(targetMonkeyId)
          monkeys.update(targetMonkeyId, targetMonkey.addItem(worryLevel))
        }
        monkeys.update(monkeyId, monkey.doneInspecting)
      }

      def keepWorryLevelsManageble(monkeyId: Int): Unit = {
        val monkey = monkeys(monkeyId)
        monkeys.update(monkeyId, monkey.keepWorryLevelsManageable(factor))
      }

      def round(): Unit = {
        monkeys.indices.foreach(turn)
        monkeys.indices.foreach(keepWorryLevelsManageble)
      }

      10000.times(round())

      val levelOfMonkeyBusiness =
        monkeys
          .map(_.inspectCount)
          .sorted
          .takeRight(2)
          .product

      levelOfMonkeyBusiness
    }

    zio.Console.printLine("Result part 1: " + resultPart1) *> // 120384
      zio.Console.printLine("Result part 2: " + resultPart2) // 32059801242
  }
}
