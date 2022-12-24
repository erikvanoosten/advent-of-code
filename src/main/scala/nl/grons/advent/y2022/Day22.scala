package nl.grons.advent.y2022

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}
import fastparse.{P, _}
import fastparse.NoWhitespace._

import java.nio.file.{Files, Path}

/**
 * See [[https://adventofcode.com/2022/day/22]].
 *
 * Warning: this solution is not generic. It only works for the example input
 * and inputs with the same shape of what is in "src/main/resources/y2022/input-day22.txt".
 */
object Day22 extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val useExampleInput = false

    val fileInput = Files.readString(Path.of("src/main/resources/y2022/input-day22.txt"))

    val exampleInput =
      """        ...#
        |        .#..
        |        #...
        |        ....
        |...#.......#
        |........#...
        |..#....#....
        |..........#.
        |        ...#....
        |        .....#..
        |        .#......
        |        ......#.
        |
        |10R5L5R10L4R5L5
        |""".stripMargin

    val input = if (useExampleInput) exampleInput else fileInput

    val mazeWidth = input.linesIterator.takeWhile(_.nonEmpty).map(_.length).max
    val maze = input
      .linesIterator
      .takeWhile(_.nonEmpty)
      .map { mazeLine =>
        // Right pad each line to get a uniform length
        mazeLine + " " * (mazeWidth - mazeLine.length)
      }
      .toIndexedSeq

    sealed trait RouteInstruction
    case class Move(distance: Int) extends RouteInstruction
    case class Turn(direction: Char) extends RouteInstruction

    val route: Seq[RouteInstruction] = {
      val routeLine = input.linesIterator.dropWhile(_.nonEmpty).drop(1).next()

      def routeParser[_: P]: P[Seq[RouteInstruction]] = P( Start ~ distanceParser ~ (turnParser ~ distanceParser).rep ~ End)
        .map(p => p._1 +: p._2.flatMap { case (turn, move) => Seq(turn, move) })
      def distanceParser[_: P]: P[Move] = P(CharsWhileIn("1234567890").!).map(v => Move(v.toInt))
      def turnParser[_: P]: P[Turn] = P( "R".! | "L".! ).map(d => Turn(d.head))

      parse(routeLine, routeParser(_)) match {
        case Parsed.Success(value, _) => value
        case _: Parsed.Failure => sys.error(s"Invalid route '$routeLine'")
      }
    }

    def mazePoint(row: Int, column: Int): Char = maze(row - 1)(column - 1)

    sealed trait Direction {
      def reverse: Direction
      def turn(turn: Turn): Direction
    }
    case object Right extends Direction {
      def reverse: Direction = Left
      def turn(turn: Turn): Direction = turn.direction match {
        case 'R' => Down
        case 'L' => Up
      }
    }
    case object Left extends Direction {
      def reverse: Direction = Right
      def turn(turn: Turn): Direction = turn.direction match {
        case 'R' => Up
        case 'L' => Down
      }
    }
    case object Up extends Direction {
      def reverse: Direction = Down
      def turn(turn: Turn): Direction = turn.direction match {
        case 'R' => Right
        case 'L' => Left
      }
    }
    case object Down extends Direction {
      def reverse: Direction = Up
      def turn(turn: Turn): Direction = turn.direction match {
        case 'R' => Left
        case 'L' => Right
      }
    }

    case class Tile(row: Int, column: Int) {
      def next(direction: Direction): Tile = direction match {
        case Down => copy(row = row + 1)
        case Left => copy(column = column - 1)
        case Right => copy(column = column + 1)
        case Up => copy(row = row - 1)
      }
      def inRange: Boolean = (1 to maze.size).contains(row) && (1 to mazeWidth).contains(column)
      def isOpen: Boolean = inRange && mazePoint(row, column) == '.'
      def isWall: Boolean = inRange && mazePoint(row, column) == '#'
      def isOffMap: Boolean = !inRange || mazePoint(row, column) == ' '
      def fold[A](ifOpen: => A, ifWall: => A, ifOffMap: => A): A = {
        if (isOpen) ifOpen
        else if (isWall) ifWall
        else ifOffMap
      }
    }

    case class Turtle(tile: Tile, facing: Direction) {
      private def move1(onWalkOffMap: Turtle => Turtle): Turtle = {
        val nexTile = tile.next(facing)
        nexTile.fold(
          ifOpen = Turtle(nexTile, facing),
          ifWall = Turtle(tile, facing),
          ifOffMap = onWalkOffMap(Turtle(tile, facing))
        )
      }
      def move(distance: Int, onWalkOffMap: Turtle => Turtle): Turtle = {
        (1 to distance).foldLeft[Turtle](this) { case (t, _) => t.move1(onWalkOffMap) }
      }
      def turn(turn: Turn): Turtle = copy(facing = facing.turn(turn))
      def password: Int = {
        val facingScore = facing match {
          case Right => 0
          case Left => 2
          case Up => 3
          case Down => 1
        }
        1000 * tile.row + 4 * tile.column + facingScore
      }
    }

    val start = Turtle(Tile(1, maze.head.indexOf('.') + 1), Right)

    val resultPart1 = {
      def wrapAround(turtle: Turtle): Turtle = {
        // wrap around to other side
        val wrapped = Iterator.iterate(turtle.tile)(_.next(turtle.facing.reverse)).takeWhile(!_.isOffMap).toSeq.last
        wrapped.fold(
          ifOpen = Turtle(wrapped, turtle.facing),
          ifWall = turtle,
          ifOffMap = sys.error("should not happen")
        )
      }

      val end = route.foldLeft(start) { case (turtle, instruction) =>
        instruction match {
          case Move(distance) => turtle.move(distance, wrapAround)
          case t: Turn => turtle.turn(t)
        }
      }
      end.password
    }

    // Part 2

    val resultPart2 = {
      val regionDimension = if (useExampleInput) 4 else 50

      case class Region(id: Int, topLeft: Tile) {
        def contains(tile: Tile): Boolean = {
          (topLeft.row until (topLeft.row + regionDimension)).contains(tile.row) &&
            (topLeft.column until (topLeft.column + regionDimension)).contains(tile.column)
        }
      }

      case class RegionConnection(
        fromRegion: Int,
        exit: Direction,
        toRegion: Int,
        arrivalDirection: Direction,
        flipOffset: Boolean
      )
      object RegionConnection {
        def directionFromChar(c: Char): Direction = c match {
          case 'R' => Right
          case 'L' => Left
          case 'U' => Up
          case 'D' => Down
        }
        def fromString(rc: String): RegionConnection = {
          RegionConnection(
            fromRegion = rc.head.toString.toInt,
            exit = directionFromChar(rc(1)),
            toRegion = rc(2).toString.toInt,
            arrivalDirection = directionFromChar(rc(3)),
            flipOffset = rc(4) == 'f'
          )
        }
      }

      // Shape of the map in the example input:
      // __#_     __1_
      // ###_     234_
      // __##     __56
      val exampleRegions: Seq[Region] =
        Seq((0,2), (1,0), (1,1), (1,2), (2,2), (2,3))
          .zipWithIndex
          .map { case ((row, col), id) =>
            Region(id + 1, Tile(row * regionDimension + 1, col * regionDimension + 1))
          }

      val exampleRegionConnections = Seq(
        "1U2Df", "1L3D-", "1R6Lf",
        "2U1Df", "2L6Uf", "2D5Uf",
        "3U1R-", "3D5Rf",
        "4R6Df",
        "5L3Uf", "5D2Uf",
        "6U4Lf", "6R1Lf", "6D2Rf"
      ).map(RegionConnection.fromString)

      // Shape of the map in the file input:
      // _##      _12
      // _#_      _3_
      // ##_      45_
      // #__      6__
      val fileRegions: Seq[Region] =
      Seq((0, 1), (0, 2), (1, 1), (2, 0), (2, 1), (3, 0))
        .zipWithIndex
        .map { case ((row, col), id) =>
          Region(id + 1, Tile(row * regionDimension + 1, col * regionDimension + 1))
        }

      val fileRegionConnections = Seq(
        "1U6R-", "1L4Rf",
        "2U6U-", "2R5Lf", "2D3L-",
        "3L4D-", "3R2U-",
        "4U3R-", "4L1Rf",
        "5R2Lf", "5D6L-",
        "6L1D-", "6D2D-", "6R5U-"
      ).map(RegionConnection.fromString)

      val regions = if (useExampleInput) exampleRegions else fileRegions
      val regionConnections = if (useExampleInput) exampleRegionConnections else fileRegionConnections

      def walkToNextCubeFace(turtle: Turtle): Turtle = {
        val turtleRegion = regions
          .find(_.contains(turtle.tile))
          .getOrElse(sys.error(s"no region contains tile ${turtle.tile}"))
        val regionConnection = regionConnections
          .find(rc => rc.fromRegion == turtleRegion.id && rc.exit == turtle.facing)
          .getOrElse(sys.error(s"unknown region connection from ${turtle.tile} (region ${turtleRegion.id}) facing ${turtle.facing}"))

        val distanceAlongEdge = turtle.facing match {
          case Right | Left => turtle.tile.row - turtleRegion.topLeft.row
          case Up | Down => turtle.tile.column - turtleRegion.topLeft.column
        }
        val targetDistanceAlongEdge =
          if (regionConnection.flipOffset) regionDimension - 1 - distanceAlongEdge
          else distanceAlongEdge

        val toRegion = regions.find(_.id == regionConnection.toRegion).getOrElse(sys.error("unknown to region"))
        val arrivalDirection = regionConnection.arrivalDirection
        val toRegionTile = arrivalDirection match {
          case Right => Tile(toRegion.topLeft.row + targetDistanceAlongEdge, toRegion.topLeft.column) // arrive on left side of region
          case Left => Tile(toRegion.topLeft.row + targetDistanceAlongEdge, toRegion.topLeft.column + regionDimension - 1) // arrive on right side of region
          case Up => Tile(toRegion.topLeft.row + regionDimension - 1, toRegion.topLeft.column + targetDistanceAlongEdge) // arrive on bottom of region
          case Down => Tile(toRegion.topLeft.row, toRegion.topLeft.column + targetDistanceAlongEdge) // arrive on top of region
        }

        toRegionTile.fold(
          ifOpen = Turtle(toRegionTile, arrivalDirection),
          ifWall = turtle,
          ifOffMap = sys.error("should not happen")
        )
      }

      val end = route.foldLeft(start) { case (turtle, instruction) =>
        instruction match {
          case Move(distance) => turtle.move(distance, walkToNextCubeFace)
          case t: Turn => turtle.turn(t)
        }
      }
      end.password
    }

    zio.Console.printLine("Result part 1: " + resultPart1) *> // 11464
      zio.Console.printLine("Result part 2: " + resultPart2) // 197122
  }

}
