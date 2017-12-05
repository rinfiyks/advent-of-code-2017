package puzzles

import util.Util

object Day1 extends App {

  private val input = Util.readInput("day1.txt").next()
  println(solve1(input))
  println(solve2(input))

  def solve1(input: String): Int = {
    val circularInput = input + input.head
    circularInput.sliding(2).filter(s => s(0) == s(1)).map(_.head.asDigit).sum
  }

  def solve2(input: String): Int = {
    input.zipWithIndex.map {
      case (c, i) => (c.asDigit, input((i + input.length / 2) % input.length).asDigit)
    }.filter(t => t._1 == t._2).map(_._1).sum
  }

}
