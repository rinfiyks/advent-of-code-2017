package puzzles

import util.Util

import scala.collection.immutable.Seq

object Day7 extends App {

  val input = Util.readInput("day7.txt").toList
  println(solve1(input))
  println(solve2(input))

  def solve1(input: Seq[String]) = {
    val strings = input.flatMap(_.replaceAll(",", "").split(" ")).filter(_.matches("^[a-zA-Z0-9]*$"))
    strings.groupBy(identity).filter(_._2.length == 1).head._1
  }

  case class RawProgram(name: String, weight: Int, children: Seq[String])

  case class Program(name: String, weight: Int, children: Seq[Program]) {
    val childrenWeight = children.map(_.totalWeight).sum
    val totalWeight: Int = weight + childrenWeight
  }

  object Program {
    // Recursively builds up tree of programs starting from a root
    def apply(name: String, rawPrograms: Seq[RawProgram]): Program = {
      val raw = rawPrograms.find(_.name == name).get
      val children = raw.children.map(n => Program(n, rawPrograms))
      Program(name, raw.weight, children)
    }
  }

  def solve2(input: Seq[String]) = {
    val rawPrograms: Seq[RawProgram] = input.map(_.replaceAll("[()\\->,]", "")).map(_.split("\\s+")).map { i =>
      RawProgram(i.head, i(1).toInt, i.drop(2).toList)
    }
    val rootName = solve1(input)
    val root = Program(rootName, rawPrograms)
    findRequiredWeight(root, root.totalWeight)
  }

  @annotation.tailrec
  def findRequiredWeight(program: Program, desiredWeight: Int): Int = {
    val childrenByWeight = program.children.groupBy(_.totalWeight)
    if (childrenByWeight.size == 1) {
      return desiredWeight - program.childrenWeight
    }
    val correctWeight = childrenByWeight.maxBy(_._2.length)._1
    val unbalancedChild = childrenByWeight.minBy(_._2.length)._2.head
    findRequiredWeight(unbalancedChild, correctWeight)
  }

}
