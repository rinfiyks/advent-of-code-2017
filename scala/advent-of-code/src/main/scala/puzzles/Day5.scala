package puzzles

import util.Util

object Day5 extends App {

  val input = Util.readInput("day5.txt").map(_.toInt).toVector
  println(solve1(input))
  println(solve2(input))

  def solve1(input: Seq[Int]): Int = {
    recurse(input, _ + 1)
  }

  def solve2(input: Seq[Int]): Int = {
    recurse(input, i => if (i >= 3) i - 1 else i + 1)
  }

  @annotation.tailrec
  private def recurse(offsets: Seq[Int], offsetFunc: Int => Int, location: Int = 0, count: Int = 0): Int = {
    if (location < 0 || location >= offsets.length) return count
    val offsetValue = offsets(location)
    val nextLocation = location + offsetValue
    recurse(offsets.updated(location, offsetFunc(offsetValue)), offsetFunc, nextLocation, count + 1)
  }

}
