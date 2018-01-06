package puzzles

import util.Util

import collection.immutable.Seq

object Day19 extends App {

  val input = Util.readInput("day19.txt").toList
  println(solve1(input))
  println(solve2(input))

  def solve1(input: Seq[String]) = {
    val initialState = State(input, Position(input.head.indexOf('|'), 0), D)
    collectLetters(initialState, List.empty).reverse.mkString
  }

  def solve2(input: Seq[String]) = {
    val initialState = State(input, Position(input.head.indexOf('|'), 0), D)
    countSteps(initialState, 0)
  }

  @annotation.tailrec
  private def collectLetters(state: State, letters: Seq[Char]): Seq[Char] = {
    state.char match {
      case ' ' => letters
      case line if "-|+".contains(line) => collectLetters(state.move, letters)
      case letter => collectLetters(state.move, letter +: letters)
    }
  }

  @annotation.tailrec
  private def countSteps(state: State, count: Int): Int = {
    state.char match {
      case ' ' => count
      case _ => countSteps(state.move, count + 1)
    }
  }

}

case class Position(x: Int, y: Int)

sealed trait Direction

object U extends Direction

object D extends Direction

object L extends Direction

object R extends Direction

case class State(diagram: Seq[String], pos: Position, dir: Direction) { // 0, 0 is top-left

  val char: Char = diagram(pos.y)(pos.x)

  def move: State = {
    val nextPos = dir match {
      case U => pos.copy(y = pos.y - 1)
      case D => pos.copy(y = pos.y + 1)
      case L => pos.copy(x = pos.x - 1)
      case R => pos.copy(x = pos.x + 1)
    }

    val nextDirection = diagram(nextPos.y)(nextPos.x) match {
      case '+' => dir match {
        case U | D =>
          val left = diagram.lift(nextPos.y).flatMap(_.lift(nextPos.x - 1))
          val right = diagram.lift(nextPos.y).flatMap(_.lift(nextPos.x + 1))
          if (left.isDefined && !left.contains(' ')) L
          else if (right.isDefined && !right.contains(' ')) R
          else dir
        case L | R =>
          val up = diagram.lift(nextPos.y - 1).flatMap(_.lift(nextPos.x))
          val down = diagram.lift(nextPos.y + 1).flatMap(_.lift(nextPos.x))
          if (up.isDefined && !up.contains(' ')) U
          else if (down.isDefined && !down.contains(' ')) D
          else dir
      }
      case _ => dir
    }

    State(diagram, nextPos, nextDirection)
  }

}
