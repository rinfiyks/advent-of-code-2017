package puzzles

import util.Util

import collection.immutable.Seq

object Day13 extends App {

  val input = Util.readInput("day13.txt").toList
  println(solve1(input))
  println(solve2(input))

  def solve1(input: Seq[String]): Int = {
    parse(input).map {
      case layer :: range :: Nil if layer % (2 * range - 2) == 0 =>
        layer * range
      case _ => 0
    }.sum
  }

  def solve2(input: Seq[String]): Int = {
    val parsedInput = parse(input)

    @annotation.tailrec
    def findDelay(delay: Int): Int = {
      if (!parsedInput.exists {
        case layer :: range :: Nil =>
          (layer + delay) % (2 * range - 2) == 0
      }) delay
      else findDelay(delay + 1)
    }

    findDelay(0)
  }

  private def parse(input: Seq[String]) = input.map(_.split(": ").map(_.toInt).toList)

}
