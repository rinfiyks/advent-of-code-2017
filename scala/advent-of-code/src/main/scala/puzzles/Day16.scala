package puzzles

import util.Util

import scala.collection.immutable.Seq

object Day16 extends App {

  val input = Util.readInput("day16.txt").next().split(",")
  println(solve1(input, 'a' to 'p'))
  println(solve2(input, 'a' to 'p'))

  def solve1(input: Array[String], letterRange: Seq[Char]) = {
    input.foldLeft(letterRange.toVector)(doDanceMove).mkString
  }

  def solve2(input: Array[String], letterRange: Seq[Char]) = {
    val cycle = findCycle(input, letterRange.toVector, List(letterRange.toVector)).reverse
    input.foldLeft(letterRange.toVector)(doDanceMove).mkString
    cycle(1000000000 % cycle.length).mkString
  }

  @annotation.tailrec
  private def findCycle(input: Array[String], letterRange: Vector[Char], programHistory: Seq[Vector[Char]]): Seq[Vector[Char]] = {
    val danceResult: Vector[Char] = input.foldLeft(letterRange.toVector)(doDanceMove)
    if (danceResult == programHistory.last) programHistory
    else findCycle(input, danceResult, danceResult +: programHistory)
  }

  private def doDanceMove(programs: Vector[Char], danceMove: String): Vector[Char] = {
    danceMove.head match {
      case 's' => spin(programs, danceMove)
      case 'x' => exchange(programs, danceMove)
      case 'p' => partner(programs, danceMove)
    }
  }

  private def spin(programs: Vector[Char], danceMove: String): Vector[Char] = {
    val size = danceMove.tail.toInt
    programs.takeRight(size) ++ programs.dropRight(size)
  }

  private def exchange(programs: Vector[Char], danceMove: String): Vector[Char] = {
    val positions = danceMove.tail.split("/").map(_.toInt)
    val program1 = programs(positions(0))
    val program2 = programs(positions(1))
    programs.updated(positions(0), program2).updated(positions(1), program1)
  }

  private def partner(programs: Vector[Char], danceMove: String): Vector[Char] = {
    val programNames = danceMove.tail.split("/").map(_(0))
    val program1Position = programs.indexOf(programNames(0))
    val program2Position = programs.indexOf(programNames(1))
    programs.updated(program1Position, programNames(1)).updated(program2Position, programNames(0))
  }

}
