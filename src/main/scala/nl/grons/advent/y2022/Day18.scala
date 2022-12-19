package nl.grons.advent.y2022

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.nio.file.{Files, Path}
import scala.annotation.tailrec

/**
 * See [[https://adventofcode.com/2022/day/18]].
 */
object Day18 extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val input = Files.readString(Path.of("src/main/resources/y2022/input-day18.txt"))

    val exampleInput = """2,2,2
                         |1,2,2
                         |3,2,2
                         |2,1,2
                         |2,3,2
                         |2,2,1
                         |2,2,3
                         |2,2,4
                         |2,2,6
                         |1,2,5
                         |3,2,5
                         |2,1,5
                         |2,3,5
                         |""".stripMargin

    case class Voxel(x: Int, y: Int, z: Int) {
      def touches(other: Voxel): Boolean =
        ((x - other.x).abs == 1 && y == other.y && z == other.z) ||
        (x == other.x && (y - other.y).abs == 1 && z == other.z) ||
        (y == other.y && x == other.x && (z - other.z).abs == 1)
    }

    val voxels: Seq[Voxel] = input
      .linesIterator
      .map(s => s.split(',').map(_.toInt))
      .map { case Array(x,y,z) => Voxel(x,y,z) }
      .toIndexedSeq

    val resultPart1 = {
      voxels
        .map { v =>
          6 - voxels.filter(_ != v).count(v.touches)
        }
        .sum
    }

    // Part 2

    val resultPart2 = {
      def findFullyEnclosedHoles(): Set[Voxel] = {
        val xRange: Range = voxels.map(_.x).min to voxels.map(_.x).max
        val yRange: Range = voxels.map(_.y).min to voxels.map(_.y).max
        val zRange: Range = voxels.map(_.z).min to voxels.map(_.z).max

        def edgeVoxel(v: Voxel): Boolean = {
          v.x == xRange.head || v.x == xRange.last ||
            v.y == yRange.head || v.y == yRange.last ||
            v.z == zRange.head || v.z == zRange.last
        }

        val potentialHolesZ: Seq[Voxel] = for {
          x <- xRange
          y <- yRange
          voxelsOnLine = voxels.filter(v => v.x == x && v.y == y).sortBy(_.z)
          if voxelsOnLine.size >= 2
          z <- (voxelsOnLine.head.z + 1) until voxelsOnLine.last.z
          v = Voxel(x, y, z)
          if !voxelsOnLine.contains(v)
        } yield v

        potentialHolesZ
          .foldLeft((Set.empty[Voxel], Set.empty[Voxel])) { case ((enclosedHoles, exposedHoles), v) =>
            if (enclosedHoles.contains(v) || exposedHoles.contains(v)) {
              // was found earlier, no need to do anything
              (enclosedHoles, exposedHoles)
            } else {
              // Use flood fill to find all holes that touch v, stop when we get out of range or hit a voxel
              @tailrec
              def flood(discovered: Set[Voxel], alreadyFlooded: Set[Voxel], exposed: Boolean): (Set[Voxel], Boolean) = {
                val xLines: Set[Voxel] = discovered.flatMap { h =>
                  Iterator.from(h.x, -1).takeWhile(xRange.contains).map(x => h.copy(x = x)).takeWhile(v => !voxels.contains(v)) ++
                    Iterator.from(h.x).takeWhile(xRange.contains).map(x => h.copy(x = x)).takeWhile(v => !voxels.contains(v))
                }
                val yLines: Set[Voxel] = discovered.flatMap { h =>
                  Iterator.from(h.y, -1).takeWhile(yRange.contains).map(y => h.copy(y = y)).takeWhile(v => !voxels.contains(v)) ++
                    Iterator.from(h.y).takeWhile(yRange.contains).map(y => h.copy(y = y)).takeWhile(v => !voxels.contains(v))
                }
                val zLines: Set[Voxel] = discovered.flatMap { h =>
                  Iterator.from(h.z, -1).takeWhile(zRange.contains).map(z => h.copy(z = z)).takeWhile(v => !voxels.contains(v)) ++
                    Iterator.from(h.z).takeWhile(zRange.contains).map(z => h.copy(z = z)).takeWhile(v => !voxels.contains(v))
                }
                val newlyDiscoveredHoles: Set[Voxel] = (xLines -- alreadyFlooded) ++ (yLines -- alreadyFlooded) ++ (zLines -- alreadyFlooded)
                val expandedNext = alreadyFlooded ++ discovered
                val exposedNext = exposed || xLines.exists(edgeVoxel) || yLines.exists(edgeVoxel) || zLines.exists(edgeVoxel)
                if (newlyDiscoveredHoles.isEmpty) (alreadyFlooded, exposedNext)
                else flood(newlyDiscoveredHoles, expandedNext, exposedNext)
              }

              val (reached, exposed) = flood(Set(v), Set.empty, exposed = false)
              if (exposed) (enclosedHoles, exposedHoles ++ reached)
              else (enclosedHoles ++ reached, exposedHoles)
            }
          }
          ._1
      }

      val holes = findFullyEnclosedHoles()
      val sidesOnInterior = voxels.map(v => holes.count(_.touches(v))).sum
      resultPart1 - sidesOnInterior
    }

    zio.Console.printLine("Result part 1: " + resultPart1) *> // 4364
      zio.Console.printLine("Result part 2: " + resultPart2) // 2508
  }
}
