package puzzles

import util.Util

object Day15 extends App {

  val input = Util.readInput("day15.txt").map(_.split(" ").last.toLong).toList
  println(solve1(input.head, input.last, 40000000))
  println(solve2(input.head, input.last, 5000000))

  private final val aFactor = 16807
  private final val bFactor = 48271

  def solve1(initialA: Long, initialB: Long, pairs: Int) = {
    (1 to pairs).foldLeft(initialA, initialB, 0) {
      case ((a, b, c), _) =>
        val aNext = a * aFactor % Integer.MAX_VALUE
        val bNext = b * bFactor % Integer.MAX_VALUE
        if (aNext % 65536 == bNext % 65536) (aNext, bNext, c + 1)
        else (aNext, bNext, c)
    }._3
  }

  def solve2(initialA: Long, initialB: Long, pairs: Int) = {
    @annotation.tailrec
    def next(value: Long, factor: Int, multiple: Int): Long = {
      val nextValue = value * factor % Integer.MAX_VALUE
      if (nextValue % multiple == 0) nextValue
      else next(nextValue, factor, multiple)
    }

    (1 to pairs).foldLeft(initialA, initialB, 0) {
      case ((a, b, c), _) =>
        val aNext = next(a, aFactor, 4)
        val bNext = next(b, bFactor, 8)
        if (aNext % 65536 == bNext % 65536) (aNext, bNext, c + 1)
        else (aNext, bNext, c)
    }._3
  }

}
