package puzzles

import util.Util

import collection.immutable.Seq

object Day24 extends App {

  val input = Util.readInput("day24.txt").toList

  println(solve1(input))
  println(solve2(input))

  def solve1(input: Seq[String]) = {
    val initialComponents: Seq[Component] = parseInput(input)
    val path = recurse1(initialComponents, List(Component(0, 0)))
    strength(path)
  }

  def solve2(input: Seq[String]) = {
    val initialComponents: Seq[Component] = parseInput(input)
    val path = recurse2(initialComponents, List(Component(0, 0)))
    strength(path)
  }

  private def recurse1(components: Seq[Component], path: Seq[Component]): Seq[Component] = {
    val possibleNextComponents = components.filter(_.matches(path.head.port2))

    possibleNextComponents.map { possibleNext =>
      recurse1(components.filterNot(_ == possibleNext), possibleNext.orient(path.head.port2) +: path)
    } match {
      case Nil => path
      case p => p.maxBy(strength)
    }
  }

  private def recurse2(components: Seq[Component], path: Seq[Component]): Seq[Component] = {
    val possibleNextComponents = components.filter(_.matches(path.head.port2))

    possibleNextComponents.map { possibleNext =>
      recurse2(components.filterNot(_ == possibleNext), possibleNext.orient(path.head.port2) +: path)
    } match {
      case Nil => path
      case p => p.sortBy(c => -strength(c)).maxBy(_.length)
    }
  }

  private def strength(components: Seq[Component]) = {
    components.map(c => c.port1 + c.port2).sum
  }

  private def parseInput(input: Seq[String]) = input.map(_.split("/").map(_.toInt)).map(l => Component(l(0), l(1)))

  case class Component(port1: Int, port2: Int) {
    def matches(port: Int): Boolean = port1 == port || port2 == port

    def orient(port: Int): Component = port match {
      case this.port1 => this
      case this.port2 => Component(port2, port1)
    }
  }

}
