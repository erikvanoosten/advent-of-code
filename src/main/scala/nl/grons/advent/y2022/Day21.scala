package nl.grons.advent.y2022

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.nio.file.{Files, Path}
import scala.annotation.tailrec

/**
 * See [[https://adventofcode.com/2022/day/21]].
 */
object Day21 extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val input = Files.readString(Path.of("src/main/resources/y2022/input-day21.txt"))

    val exampleInput =
      """root: pppw + sjmn
        |dbpl: 5
        |cczh: sllz + lgvd
        |zczc: 2
        |ptdq: humn - dvpt
        |dvpt: 3
        |lfqf: 4
        |humn: 5
        |ljgn: 2
        |sjmn: drzm * dbpl
        |sllz: 4
        |pppw: cczh / lfqf
        |lgvd: ljgn * ptdq
        |drzm: hmdt - zczc
        |hmdt: 32
        |""".stripMargin

    sealed trait Monkey {
      def name: String
    }
    case class NumberMonkey(name: String, n: Int) extends Monkey
    case class OperationMonkey(name: String, lhs: String, op: Char, rhs: String) extends Monkey

    val NumberMonkeyRE = raw"([a-z]{4}): ([0-9]+)".r
    val OperationMonkeyRE = raw"([a-z]{4}): ([a-z]{4}) ([-+*/]) ([a-z]{4})".r

    val monkeys: Map[String, Monkey] = input
      .linesIterator
      .map {
        case NumberMonkeyRE(name, n) => NumberMonkey(name, n.toInt)
        case OperationMonkeyRE(name, lhs, op, rhs) => OperationMonkey(name, lhs, op.head, rhs)
      }
      .map(m => m.name -> m)
      .toMap

    def calculate(name: String): BigInt = {
      monkeys(name) match {
        case NumberMonkey(_, n) => n
        case OperationMonkey(_, lhs, op, rhs) =>
          val l = calculate(lhs)
          val r = calculate(rhs)
          op match {
            case '+' => l + r
            case '-' => l - r
            case '*' => l * r
            case '/' => l / r
          }
      }
    }

    val resultPart1 = {
      calculate("root")
    }

    // Part 2

    val resultPart2 = {
      def dependsOnHumn(monkeyName: String): Boolean = {
        if (monkeyName == "humn") true
        else {
          monkeys(monkeyName) match {
            case NumberMonkey(_, _) => false
            case OperationMonkey(_, lhs, _, rhs) => dependsOnHumn(lhs) || dependsOnHumn(rhs)
          }
        }
      }

      @tailrec
      def deriveHumnNumber(monkeyName: String, target: BigInt): BigInt = {
        if (monkeyName == "humn") target
        else {
          monkeys(monkeyName) match {
            case OperationMonkey(_, lhs, op, rhs) if dependsOnHumn(lhs) =>
              val rvalue = calculate(rhs)
              val ltarget = op match {
                case '+' => target - rvalue
                case '-' => target + rvalue
                case '*' => target / rvalue
                case '/' => target * rvalue
              }
              deriveHumnNumber(lhs, ltarget)
            case OperationMonkey(_, lhs, op, rhs) =>
              val lvalue = calculate(lhs)
              val rtarget = op match {
                case '+' => target - lvalue
                case '-' => lvalue - target
                case '*' => target / lvalue
                case '/' => lvalue / target
              }
              deriveHumnNumber(rhs, rtarget)
            case NumberMonkey(_, _) => sys.error("should not happen!")
          }
        }
      }

      val root = monkeys("root").asInstanceOf[OperationMonkey]
      if (dependsOnHumn(root.lhs)) deriveHumnNumber(root.lhs, calculate(root.rhs))
      else deriveHumnNumber(root.rhs, calculate(root.lhs))
    }

    zio.Console.printLine("Result part 1: " + resultPart1) *> // 31017034894002
      zio.Console.printLine("Result part 2: " + resultPart2) // 3555057453229
  }

}
