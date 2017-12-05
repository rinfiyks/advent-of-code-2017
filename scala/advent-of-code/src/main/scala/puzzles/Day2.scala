package puzzles

import util.Util

object Day2 extends App {

  val input = parseInput(Util.readInput("day2.txt").toList)

  println(solve1(input))
  println(solve2(input))

  def solve1(input: Seq[Seq[Int]]) = {
    input.map(rowDifference).sum
  }

  def rowDifference(row: Seq[Int]) = {
    row.max - row.min
  }

  def solve2(input: Seq[Seq[Int]]) = {
    input.map(divisionOfTheTwoThatAreDivisible).sum
  }

  def divisionOfTheTwoThatAreDivisible(row: Seq[Int]) = {
    val successfulDivisions: Seq[Int] = for {
      i <- row
      j <- row if j != i
      d <- i % j match {
        case 0 => Some(i / j)
        case _ => None
      }
    } yield d

    // This Seq should always have one element according to the question spec, but just to be safe, sum it
    successfulDivisions.sum
  }

  def parseInput(input: Seq[String]): Seq[List[Int]] = {
    input.map(_.split("\\s+").toList).map(_.map(_.toInt))
  }

}
