package nl.grons.advent.y2022

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.nio.file.{Files, Path}
import scala.collection.mutable

/**
 * See [[https://adventofcode.com/2022/day/7]].
 */
object Day7 extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val fileContent = Files.readString(Path.of("src/main/resources/y2022/input-day7.txt"))

    sealed abstract class Node {
      def name: String
      def size: Long
    }
    class File(val name: String, var size: Long) extends Node
    class Dir(val name: String, var nodes: Seq[Node]) extends Node {
      def size: Long = nodes.map(_.size).sum
      def contains(name: String): Boolean = nodes.exists(_.name == name)
      def find(name: String): Option[Node] = nodes.find(_.name == name)
      def addOrUpdateFile(file: File): Unit = {
        nodes = nodes.filter(_.name != file.name) :+ file
      }
      def addOrGetDir(dirName: String): Dir = {
        find(dirName) match {
          case Some(existingDir: Dir) => existingDir
          case None =>
            val newDir = new Dir(dirName, Seq.empty)
            nodes = nodes :+ newDir
            newDir
          case _ => sys.error(s"$dirName is an already existing file")
        }
      }
    }

    val root = new Dir("/", Seq.empty)
    var pwd = List(root)
    fileContent
      .lines()
      .forEach { line =>
        if (line.startsWith("$")) {
          val words = line.split(" ", 3)
          assert(words(0) == "$")
          if (words(1) == "cd") {
            pwd = words(2) match {
              case "/" => List(root)
              case ".." => pwd.tail
              case subDirName => pwd.head.addOrGetDir(subDirName) :: pwd
            }
          }
        } else if (line.startsWith("dir")) {
          // add dir
          val Array("dir", name) = line.split(" ", 2)
          pwd.head.addOrGetDir(name)
        } else {
          // add file
          val Array(size, name) = line.split(" ", 2)
          pwd.head.addOrUpdateFile(new File(name, size.toLong))
        }
      }

    class DescendingDepthFirstIterator(root: Dir) extends Iterator[Node] {
      private val dirIters = new mutable.Stack[Iterator[Node]]()
      dirIters.push(root.nodes.iterator)

      override def hasNext: Boolean = dirIters.exists(_.hasNext)

      override def next(): Node = {
        while (dirIters.nonEmpty && !dirIters.top.hasNext) {
          // Current dir exhausted, jump to parent directory
          dirIters.pop()
        }
        if (dirIters.isEmpty) throw new NoSuchElementException()
        val node = dirIters.top.next()
        node match {
          case dir: Dir => dirIters.push(dir.nodes.iterator)
          case file: File => ()
        }
        node
      }
    }

    val resultPart1 = new DescendingDepthFirstIterator(root)
      .filter(_.isInstanceOf[Dir])
      .map(_.size)
      .filter(_ <= 100000)
      .sum

    val capacity = 70000000L
    val used = root.size
    val unused = capacity - used
    val needed = 30000000L
    val toFreeUp = needed - unused

    val resultPart2 = new DescendingDepthFirstIterator(root)
      .filter(_.isInstanceOf[Dir])
      .filter(_.size >= toFreeUp)
      .toSeq
      .minBy(_.size)
      .size

    zio.Console.printLine("Result part 1: " + resultPart1) *> // 2031851
    zio.Console.printLine("Result part 2: " + resultPart2) // 2568781
  }

}
