package puzzles

import util.Util

import collection.immutable.Seq

object Day21 extends App {

  val input = Util.readInput("day21.txt").toList

  println(solve1(input, 5))
  println(solve2(input, 18))

  def solve1(input: Seq[String], iterations: Int) = {
    val rules = parseInput(input).flatMap(_.flipsAndRotations)
    val initialGrid =
      """
        |.#.
        |..#
        |###
      """.trim.stripMargin.split("\\n").toList

    (1 to iterations).foldLeft(initialGrid) {
      case (grid, _) => enhanceGrid(grid, rules).toList
    }.mkString.count(_ == '#')
  }

  def solve2(input: Seq[String], iterations: Int) = solve1(input, iterations)

  private def enhanceGrid(grid: Seq[String], rules: Seq[Rule]): Seq[String] = {
    val size = if (grid.length % 2 == 0) 2 else 3

    grid.grouped(size).map {
      _.map(_.grouped(size).toList).transpose.map(p => applyRule(p, rules))
    }.flatMap(_.reduce((l1, l2) => (l1 zip l2).map { case (s1, s2) => s1 + s2 })).toList
  }

  private def applyRule(pattern: Seq[String], rules: Seq[Rule]): Seq[String] = {
    rules.find(_.input == pattern) match {
      case Some(rule) => rule.output
      case None => pattern // should never happen
    }
  }

  private def parseInput(input: Seq[String]): Seq[Rule] = {
    input.map { l =>
      val split = l.split(" => ").map(_.split("/").toList)
      Rule(split(0), split(1))
    }
  }

}

case class Rule(input: Seq[String], output: Seq[String]) {

  def flipsAndRotations: Set[Rule] = {
    val hFlip = input.map(_.reverse)
    val vFlip = input.reverse
    val flips = Set(input, hFlip, vFlip)

    flips.flatMap { rot1 =>
      val rot2 = rotate(rot1)
      val rot3 = rotate(rot2)
      val rot4 = rotate(rot3)
      Set(rot1, rot2, rot3, rot4)
    }.map(i => Rule(i, output))
  }

  private def rotate(i: Seq[String]) = i.reverse.transpose.map(_.mkString)

}
