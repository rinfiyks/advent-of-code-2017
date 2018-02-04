package puzzles

import util.Util

import scala.collection.immutable.Seq

object Day25 extends App {

  val input = Util.readInput("day25.txt").toList

  println(solve1(input))

  def solve1(input: Seq[String]) = {
    val initialTM: TuringMachine = parseInput(input)
    (1 to input(1).split(" ")(5).toInt).foldLeft(initialTM) {
      (tm, _) => tm.step
    }.checksum
  }

  private def parseInput(input: Seq[String]): TuringMachine = {
    val states: Seq[State] = input.drop(2).grouped(10).toList.map(parseStateInput)
    val stateName: String = input.head.dropRight(1).split(" ")(3)

    TuringMachine(Tape(Vector.empty, Vector(0)), 0, states, stateName)
  }

  private def parseStateInput(input: Seq[String]): State = {
    val name = input(1).dropRight(1).split(" ")(2)
    val rules = input.drop(2).grouped(4).toList.map(parseRule)
    State(name, rules)
  }

  private def parseRule(input: Seq[String]): Rule = {
    val activationValue = input.head.dropRight(1).split(" ").last.toInt
    val nextValue = input(1).dropRight(1).split(" ").last.toInt
    val movement = input(2).dropRight(1).split(" ").last match {
      case "left" => -1
      case "right" => 1
    }
    val nextStateName = input(3).dropRight(1).split(" ").last
    Rule(activationValue, nextValue, movement, nextStateName)
  }

  case class TuringMachine(tape: Tape, cursor: Int, states: Seq[State], currentState: String) {

    def step: TuringMachine = {
      states.find(_.name == currentState) flatMap {
        _.rules.find(_.activationValue == tape.value(cursor))
      } match {
        case Some(rule) =>
          val nextCursor = cursor + rule.movement
          val newTape = tape.update(cursor, rule.nextValue).resizeIfNecessary(nextCursor)
          TuringMachine(newTape, nextCursor, states, rule.nextStateName)
      }
    }

    def checksum: Int = tape.left.count(_ == 1) + tape.right.count(_ == 1)

  }

  // left is all negative positions, right is all positive positions
  // left(0) is pos -1, left(1) is pos -2, etc.
  case class Tape(left: Seq[Int], right: Seq[Int]) {

    def value(cursor: Int): Int =
      if (cursor < 0) left(-1 - cursor) else right(cursor)

    def update(cursor: Int, value: Int): Tape =
      if (cursor < 0)
        Tape(left.updated(-1 - cursor, value), right)
      else
        Tape(left, right.updated(cursor, value))

    def resizeIfNecessary(cursor: Int): Tape =
      if (cursor < -left.length)
        Tape(left :+ 0, right)
      else if (cursor >= right.length)
        Tape(left, right :+ 0)
      else this

  }

  case class State(name: String, rules: Seq[Rule])

  case class Rule(activationValue: Int, nextValue: Int, movement: Int, nextStateName: String)

}
