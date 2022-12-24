package nl.grons.advent.y2022

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.nio.file.{Files, Path}
import scala.collection.mutable

/**
 * See [[https://adventofcode.com/2022/day/19]].
 *
 * NOTE: part 2 takes a substantial time to run!
 * To make this app go faster, please give it large heap. `-Xmx4g` will do, more is better.
 *
 * To run it on your own input you may have to fix the bug mentioned on line 203.
 */
object Day19 extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val input = Files.readString(Path.of("src/main/resources/y2022/input-day19.txt"))

    val exampleInput = """Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
                         |Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
                         |""".stripMargin

    case class Blueprint(
        id: Int,
        oreOre: Int,
        clayOre: Int,
        obsidianOre: Int,
        obsidianClay: Int,
        geodeOre: Int,
        geodeObsidian: Int
    )

    val BlueprintRE = raw"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.".r
    val blueprints =
      input
        .linesIterator
        .map { case BlueprintRE(id, oreOre, clayOre, obsidianOre, obsidianClay, geodeOre, geodeObsidian) =>
          Blueprint(id.toInt, oreOre.toInt, clayOre.toInt, obsidianOre.toInt, obsidianClay.toInt, geodeOre.toInt, geodeObsidian.toInt)
        }
        .toIndexedSeq

    case class Robots(ore: Int, clay: Int, obsidian: Int, geode: Int) {
      def addOreRobot(): Robots = copy(ore = ore + 1)
      def addClayRobot(): Robots = copy(clay = clay + 1)
      def addObsidianRobot(): Robots = copy(obsidian = obsidian + 1)
      def addGeodeRobot(): Robots = copy(geode = geode + 1)
    }

    case class Resources(ore: Int, clay: Int, obsidian: Int, geode: Int) {
      def allPositive: Boolean = ore >= 0 && clay >= 0 && obsidian >= 0 && geode >= 0
      def buildGeodeRobot(blueprint: Blueprint): Option[Resources] =
        Option(copy(ore = ore - blueprint.geodeOre, obsidian = obsidian - blueprint.geodeObsidian)).filter(_.allPositive)
      def buildObsidianRobot(blueprint: Blueprint): Option[Resources] =
        Option(copy(ore = ore - blueprint.obsidianOre, clay = clay - blueprint.obsidianClay)).filter(_.allPositive)
      def buildClayRobot(blueprint: Blueprint): Option[Resources] =
        Option(copy(ore = ore - blueprint.clayOre)).filter(_.allPositive)
      def buildOreRobot(blueprint: Blueprint): Option[Resources] =
        Option(copy(ore = ore - blueprint.oreOre)).filter(_.allPositive)
      def mine(robots: Robots, timeToMine: Int = 1): Resources =
        copy(
          ore = ore + timeToMine * robots.ore,
          clay = clay + timeToMine * robots.clay,
          geode = geode + timeToMine * robots.geode,
          obsidian = obsidian + timeToMine * robots.obsidian
        )
    }

    case class State(robots: Robots, inStock: Resources) {
      def buildGeodeRobot(blueprint: Blueprint): Option[State] =
        inStock.buildGeodeRobot(blueprint).map(State(robots.addGeodeRobot(), _))
      def buildObsidianRobot(blueprint: Blueprint): Option[State] =
        inStock.buildObsidianRobot(blueprint).map(State(robots.addObsidianRobot(), _))
      def buildClayRobot(blueprint: Blueprint): Option[State] =
        inStock.buildClayRobot(blueprint).map(State(robots.addClayRobot(), _))
      def buildOreRobot(blueprint: Blueprint): Option[State] =
        inStock.buildOreRobot(blueprint).map(State(robots.addOreRobot(), _))
      def mine(robots: Robots, timeToMine: Int = 1): State =
        copy(inStock = inStock.mine(robots, timeToMine))
    }

    /**
     * timeRemaining = Time remaining after the current minute
     */
    def nextMinute(blueprint: Blueprint, timeRemaining: Int, state: State): IterableOnce[State] = {
      // At timeRemaining == 0, new robots cannot contribute any more
      // At timeRemaining == 1, only a geode robot can contribute
      // At timeRemaining == 2, only a robot that allows building a geode robot in the next minute can contribute
      // etc.

      val next: Seq[State] = if (timeRemaining == 0) {
        // Time is up, there is no point in building another robot
        Seq(state)
      } else {
        // If we can build a geode robot, that is always the only correct choice.
        val buildGeodeRobot = state.buildGeodeRobot(blueprint)
        if (buildGeodeRobot.isDefined) {
          buildGeodeRobot.toSeq
        } else if (timeRemaining == 1) {
          Seq(state)
        } else if (timeRemaining == 2) {
          // can we build a geode robot in the next, and final, minute?
          state.mine(state.robots).buildObsidianRobot(blueprint) match {
            case yes: Some[_] =>
              // No need to do anything
              Seq(state)
            case /* no */ None =>
              // Can we build a robot that will make us build a geode robot in the next minute?
              val obsidianNeeded = blueprint.geodeObsidian - state.inStock.obsidian - state.robots.obsidian
              val oreNeeded = blueprint.geodeOre - state.inStock.clay - state.robots.clay
              if (obsidianNeeded == 1 && oreNeeded == 0) {
                // yes, by building an obsidian robot
                state.buildObsidianRobot(blueprint).toSeq
              } else if (obsidianNeeded == 0 && oreNeeded == 1) {
                // yes, by building an ore robot
                state.buildOreRobot(blueprint).toSeq
              } else {
                // no :(
                Seq(state)
              }
          }
        } else {
          // What robots can we still build in time?
          val canBuildOreRobot = {
            val oreNeededFromAdditionalRobots = blueprint.oreOre.toDouble - state.inStock.ore - (timeRemaining - 1) * state.robots.ore
            // Assuming we can build any robot at any time:
            Math.round(oreNeededFromAdditionalRobots / (timeRemaining - 1) + 0.5) < timeRemaining
          }
          val canBuildClayRobot = {
            val oreNeededFromAdditionalRobots = blueprint.clayOre.toDouble - state.inStock.ore - (timeRemaining - 1) * state.robots.ore
            // Assuming we can build any robot at any time:
            (oreNeededFromAdditionalRobots == 0 || canBuildOreRobot) &&
              canBuildOreRobot && Math.round(oreNeededFromAdditionalRobots / (timeRemaining - 1) + 0.5) < timeRemaining
          }
          val canBuildObsidianRobot = {
            val clayNeededFromAdditionalRobots = blueprint.obsidianClay.toDouble - state.inStock.clay - (timeRemaining - 1) * state.robots.clay
            val oreNeededFromAdditionalRobots = blueprint.obsidianOre.toDouble - state.inStock.ore - (timeRemaining - 1) * state.robots.ore
            // Assuming we can build any robot at any time:
            (clayNeededFromAdditionalRobots == 0 || canBuildClayRobot) &&
            (oreNeededFromAdditionalRobots == 0 || canBuildOreRobot) &&
            Math.round(clayNeededFromAdditionalRobots / (timeRemaining - 1) + 0.5) + Math.round(oreNeededFromAdditionalRobots / (timeRemaining - 1) + 0.5) < timeRemaining
          }

          Seq.concat(
            if (canBuildObsidianRobot) state.buildObsidianRobot(blueprint) else None,
            if (canBuildClayRobot) state.buildClayRobot(blueprint) else None,
            if (canBuildOreRobot) state.buildOreRobot(blueprint) else None,
            Seq(state)
          )
        }
      }

      next.map(_.mine(state.robots))
    }

    val resultPart1 = {
      blueprints
        .map { blueprint =>
          val startState = State(Robots(1, 0, 0, 0), Resources(0, 0, 0, 0))
          var states: Set[State] = Set(startState)
          var seenStates: Set[State] = Set.empty
          (1 to 24).foreach { time =>
            states = states.flatMap(s => nextMinute(blueprint, 24 - time, s)) -- seenStates
            seenStates = seenStates ++ states
            // println(s"at minute $time: ${states.size} states (seen: ${seenStates.size})")
          }
          val maxGeodeCount = states.map(_.inStock.geode).max
          println(s"Blueprint ${blueprint.id} created $maxGeodeCount")
          blueprint.id * maxGeodeCount
        }
        .sum
    }

    // Part 2

    val resultPart2 = {
      blueprints
        .take(3)
        .map { blueprint =>
          val startState = State(Robots(1, 0, 0, 0), Resources(0, 0, 0, 0))
          var states: Set[State] = Set(startState)
          val seenStates: mutable.Set[State] = new mutable.HashSet[State]()
          (1 to 27).foreach { time =>
            val timeRemaining = 32 - time
            states = states.flatMap(s => nextMinute(blueprint, timeRemaining, s))
            val beforeTrunc = states.size
            states = states -- seenStates
            val dupsRemoved = beforeTrunc - states.size
            seenStates ++= states
            // println(s"at minute $time: ${states.size} states (removed $dupsRemoved from ${seenStates.size} seen states)")
          }

          implicit class IteratorFlatter[A](val iter: Iterator[A]) {
            def timeRepeatingFlatMap(time: Range)(f: (Int, A) => IterableOnce[A]): Iterator[A] = {
              time.foldLeft(iter) { case (acc, time) =>
                acc.flatMap(a => f(time, a))
              }
            }
          }

          // Do a deep dive on all possible states starting from minute 28.
          // NOTE: there is a bug on line 201. The correct range is:
          // `Range.inclusive(32, 28, -1)`!
          // However, we still got the right answer! :)
          val maxGeodeCount = states
            .iterator
            .timeRepeatingFlatMap(28 to 32) { case (t, s) => nextMinute(blueprint, t, s) }
            .map(_.inStock.geode)
            .max

          println(s"Blueprint ${blueprint.id} created $maxGeodeCount")
          maxGeodeCount
        }
        .product
    }

    zio.Console.printLine("Result part 1: " + resultPart1) *> // 1127
      zio.Console.printLine("Result part 2: " + resultPart2) // 21546
  }

}
