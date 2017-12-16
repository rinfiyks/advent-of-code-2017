package puzzles

import util.Util

import collection.immutable.Seq

object Day12 extends App {

  val input = Util.readInput("day12.txt").toList
  println(solve1(input))
  println(solve2(input))

  def solve1(input: Seq[String]) = {
    val programs: Seq[Program] = parseInput(input)
    findPrograms(programs.head, programs, Set.empty[Program]).size
  }

  def solve2(input: Seq[String]) = {
    val programs: Seq[Program] = parseInput(input)
    programs.foldLeft(Set.empty[Program], 0) {
      case ((c, n), p) if !c.contains(p) => (c ++ findPrograms(p, programs, Set.empty[Program]), n + 1)
      case (t, _) => t
    }._2
  }

  private def findPrograms(program: Program, allPrograms: Seq[Program], connectedPrograms: Set[Program]): Set[Program] = {
    val directConnections = program.channels.flatMap(id => allPrograms.find(_.id == id)).toSet
    val newPrograms = directConnections -- connectedPrograms
    newPrograms.foldLeft(connectedPrograms) {
      case (c, p) => findPrograms(p, allPrograms, c + p)
    }
  }

  case class Program(id: String, channels: Seq[String])

  private def parseInput(input: Seq[String]) = {
    input.map { line =>
      val s = line.split(" <-> ")
      Program(s.head, s(1).split(", ").toList)
    }
  }

}
