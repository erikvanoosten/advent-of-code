package nl.grons.advent.y2022

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.nio.file.{Files, Path}
import scala.collection.mutable

/**
 * See [[https://adventofcode.com/2022/day/16]].
 */
object Day16 extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val input = Files.readString(Path.of("src/main/resources/y2022/input-day16.txt"))

    val input1 = """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
                  |Valve BB has flow rate=13; tunnels lead to valves CC, AA
                  |Valve CC has flow rate=2; tunnels lead to valves DD, BB
                  |Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
                  |Valve EE has flow rate=3; tunnels lead to valves FF, DD
                  |Valve FF has flow rate=0; tunnels lead to valves EE, GG
                  |Valve GG has flow rate=0; tunnels lead to valves FF, HH
                  |Valve HH has flow rate=22; tunnel leads to valve GG
                  |Valve II has flow rate=0; tunnels lead to valves AA, JJ
                  |Valve JJ has flow rate=21; tunnel leads to valve II
                  |""".stripMargin

    case class Valve(name: String, releaseRate: Int, leadingTo: Seq[String])

    val ValveRE = raw"Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? ([A-Z, ]+)".r
    val valves: Seq[Valve] = input
      .linesIterator
      .map { case ValveRE(name, rate, leading) =>
        Valve(name, rate.toInt, leading.split(", "))
      }
      .toIndexedSeq
    val valvesByName: Map[String, Valve] =
      valves
        .map(v => v.name -> v)
        .toMap
    val startValve = valvesByName("AA")

    // Bellman-Ford algorithm
    def routesFrom(start: Valve): Map[Valve, Seq[Valve]] = {
      val distanceByDestination: mutable.Map[Valve, Int] =
        mutable.Map.empty.withDefault(_ => Int.MaxValue)
      distanceByDestination.put(start, 0)
      val predecessors: mutable.Map[Valve, Valve] = mutable.Map.empty
      val visited: mutable.Set[Valve] = mutable.Set.empty
      visited += start

      val inProgress: mutable.Queue[Valve] = mutable.Queue.empty
      inProgress += start

      while (inProgress.nonEmpty) {
        val position = inProgress.dequeue()
        val shortestDistance = distanceByDestination(position)
        position
          .leadingTo
          .map(valvesByName(_))
          .foreach { next =>
            if (!visited.contains(next)) {
              visited += next
              distanceByDestination.put(next, shortestDistance + 1)
              predecessors.put(next, position)
              inProgress += next
            }
        }
      }

      valves
        .filter(_ != start)
        .map { toValve =>
          val path = Iterator
            .unfold(toValve) { dest =>
              predecessors.get(dest).map(d => (d, d))
            }
            .toSeq
            .reverse
            .drop(1) :+ toValve  // drop the starting node, add the destination node

          toValve -> path
        }
        .toMap
    }

    val allRoutes: Map[(String, String), Seq[Valve]] = valves
      .flatMap(from => routesFrom(from).map { case (to, path) => (from.name, to.name) -> path })
      .toMap

    val allDistances: Map[String, Int] =
      allRoutes
        .map { case ((from, to), path) =>
          s"$from.$to" -> path.size
        }
    def mapKeyV(from: Valve, to: Valve): String = mapKeyS(from.name, to.name)
    def mapKeyS(from: String, to: String): String = s"$from.$to"

    def printDistances(valves: Seq[Valve]): Unit = {
      println("   " + valves.map(_.name).mkString(" "))
      valves.map(_.name).foreach { namex =>
        val distances = valves.map(_.name).map(namey => allDistances.getOrElse(mapKeyS(namex, namey), 0))
        println(s"$namex " + distances.map(d => if (d == 0) "  " else "%2d".format(d)).mkString(" "))
      }
    }
    // printDistances(startValve +: valvesToOpen.sortBy(_.name))

    val valvesToOpen: Set[Valve] = valves.filter(_.releaseRate > 0).toSet

    def maxFlow(totalTime: Int, valve: Valve, flow: Int, time: Int, valvesToOpen: Set[Valve]): Int = {
      if (time >= totalTime || valvesToOpen.isEmpty) {
        flow
      } else {
        valvesToOpen
          .iterator
          .map { nextValve =>
            // First move to nextValve
            val travelTime = allDistances(mapKeyV(valve, nextValve))
            // Then open the valve
            val timeRemainingAfterTravel = totalTime - time - travelTime
            val (openTime, flowIncrease) =
              if (nextValve.releaseRate > 0) (1, timeRemainingAfterTravel * nextValve.releaseRate)
              else (0, 0)
            val flowNext = flow + flowIncrease
            maxFlow(totalTime, nextValve, flowNext, time + openTime + travelTime, valvesToOpen - nextValve)
          }
          .max
      }
    }

    val resultPart1 = {
      maxFlow(30, startValve, 0, 1, valvesToOpen)
    }

    // Part 2

    val resultPart2 = {
      valvesToOpen
        .subsets()
        .map { valvesToOpenYou =>
          val valvesToOpenElephant = valvesToOpen -- valvesToOpenYou

          maxFlow(26, startValve, 0, 1, valvesToOpenYou) +
          maxFlow(26, startValve, 0, 1, valvesToOpenElephant)
        }
        .max
    }

    zio.Console.printLine("Result part 1: " + resultPart1) *> // 1923
      zio.Console.printLine("Result part 2: " + resultPart2) // 2594
  }
}
