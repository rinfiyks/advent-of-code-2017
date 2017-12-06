package puzzles

import util.Util

import scala.collection.immutable.Seq
import scala.collection.mutable.ArrayBuffer

object Day6 extends App {

  val input = Util.readInput("day6.txt").next().split("\\t").map(_.toInt).toList
  println(solve1(input))
  println(solve2(input))

  def solve1(input: Seq[Int]) = {
    recurse(Seq.empty[Seq[Int]], input, (s, _) => s.length + 1)
  }

  def solve2(input: Seq[Int]) = {
    recurse(Seq.empty[Seq[Int]], input, (s, c) => s.length - s.indexOf(c) + 1)
  }

  @annotation.tailrec
  private def recurse(previousStates: Seq[Seq[Int]], currentState: Seq[Int], answerFunc: (Seq[Seq[Int]], Seq[Int]) => Int): Int = {
    val newState = updateState(currentState)
    if (previousStates contains newState) answerFunc(previousStates, newState)
    else recurse(previousStates :+ currentState, newState, answerFunc)
  }

  private def updateState(state: Seq[Int]): Seq[Int] = {
    val buf = new ArrayBuffer[Int]
    state.copyToBuffer(buf)

    val max = state.max
    var i = state.indexOf(max)
    buf(i) = 0

    (0 until max) foreach { _ =>
      i = (i + 1) % state.length
      buf(i) = buf(i) + 1
    }

    buf.toList
  }

}
