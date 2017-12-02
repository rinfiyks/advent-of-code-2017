package puzzles

import util.Util

object Day1 extends App {

  println(solve1(Util.readInput("day1.txt").head))
  println(solve2(Util.readInput("day1.txt").head))

  def solve1(input: String): Int = {
    val circularInput = input.head + input
    circularInput.sliding(2).filter(s => s(0) == s(1)).map(_.head.asDigit).sum
  }

  def solve2(input: String): Int = {
    input.zipWithIndex.map {
      case (c, i) => (c.asDigit, input((i + input.length / 2) % input.length).asDigit)
    }.filter(t => t._1 == t._2).map(_._1).sum
  }

}
