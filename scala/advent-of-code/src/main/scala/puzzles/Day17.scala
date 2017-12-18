package puzzles

import util.Util

object Day17 extends App {

  val input = Util.readInput("day17.txt").next().toInt
  println(solve1(input, 2017))
  println(solve2(input, 50000000))

  def solve1(stepCount: Int, spins: Int) = {
    val (finalBuffer, finalPosition) = runSpinlock(stepCount, spins)
    finalBuffer(finalPosition % finalBuffer.length)
  }

  def solve2(stepCount: Int, spins: Int) = {
    (1 to spins).foldLeft(0, 0) {
      case ((currentPosition, numberAfter0), i) =>
        val nextPosition = (currentPosition + stepCount) % i
        if (nextPosition == 0) (nextPosition + 1, i)
        else (nextPosition + 1, numberAfter0)
    }._2
  }

  private def runSpinlock(stepCount: Int, spins: Int) = {
    (1 to spins).foldLeft(Vector(0), 0) {
      case ((buffer, position), i) =>
        spinlockStep(buffer, position, stepCount, i)
    }
  }

  private def spinlockStep(buffer: Vector[Int], position: Int, stepCount: Int, valueToInsert: Int): (Vector[Int], Int) = {
    val newPosition = (position + stepCount) % buffer.length
    val (left, right) = buffer.splitAt(newPosition)
    val newBuffer = left ++ Vector(valueToInsert) ++ right
    (newBuffer, newPosition + 1)
  }

}
