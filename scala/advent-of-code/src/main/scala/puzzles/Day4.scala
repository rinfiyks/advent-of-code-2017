package puzzles

import util.Util

object Day4 extends App {

  val input = Util.readInput("day4.txt").toList
  println(solve1(input))
  println(solve2(input))

  def solve1(input: Seq[String]) = {
    input.count { l =>
      val a = l.split(" ")
      a.length == a.toSet.size
    }
  }

  def solve2(input: Seq[String]) = {
    input.count { l =>
      val a = l.split(" ").map(_.sorted)
      a.length == a.toSet.size
    }
  }

}
