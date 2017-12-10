package puzzles

import util.Util

import collection.immutable.Seq

object Day10 extends App {

  val input = Util.readInput("day10.txt").next()
  println(solve1(input, 256))
  println(solve2(input, 256))

  def solve1(input: String, circleSize: Int) = {
    val initialList = (0 until circleSize).toList
    val lengths = input.split(",").map(_.toInt).toList

    val result = knotHashRound(initialList, lengths, initialPosition = 0, initialSkipSize = 0)._1
    result.head * result(1)
  }

  def solve2(input: String, circleSize: Int) = {
    val initialList = (0 until circleSize).toList
    val lengths = input.map(_.toInt) ++ List(17, 31, 73, 47, 23)

    val sparseHash = (1 to 64).foldLeft[(Seq[Int], Int, Int)]((initialList, 0, 0)) {
      case ((list, position, skipSize), _) =>
        val (newList, newPos, newSkipSize) = knotHashRound(list, lengths, position, skipSize)
        (newList, newPos, newSkipSize)
    }._1
    val denseHash = sparseHash.grouped(16).map(_.reduce(_ ^ _)).toList
    denseHash.map("%02x".format(_)).mkString
  }

  private def knotHashRound(initialList: Seq[Int], lengths: Seq[Int], initialPosition: Int, initialSkipSize: Int) = {
    val circleSize = initialList.length
    lengths.foldLeft(initialList, initialPosition, initialSkipSize) {
      case ((list, pos, skipSize), sectionLength) =>
        val doubleList = list ++ list
        val section = doubleList.slice(pos, pos + sectionLength)

        val updatedDoubleList = (doubleList.take(pos) ++ section.reverse
          ++ doubleList.takeRight(circleSize * 2 - pos - sectionLength))

        val updatedList = (updatedDoubleList.slice(circleSize, pos + sectionLength)
          ++ updatedDoubleList.slice(pos + sectionLength - circleSize, circleSize))

        (updatedList, (pos + sectionLength + skipSize) % circleSize, skipSize + 1)
    }
  }

}