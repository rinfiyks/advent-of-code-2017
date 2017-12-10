package puzzles

import util.Util

object Day9 extends App {

  val input = Util.readInput("day9.txt").next()
  println(solve1(input))
  println(solve2(input))

  def solve1(input: String): Int = {
    parse(input)._1.collect { case s: Bracket => s }.map(_.score).sum
  }

  def solve2(input: String): Int = {
    parse(input)._2
  }

  private def parse(input: String) = {
    input.foldLeft[(List[Symbol], Int, Symbol)]((List.empty[Symbol], 0, Bracket(0))) {
      case ((symbols, count, Bracket(s)), c) =>
        c match {
          case '}' => (Bracket(s) +: symbols, count, Bracket(s - 1))
          case '{' => (symbols, count, Bracket(s + 1))
          case '<' => (symbols, count, Garbage(s))
          case _ => (symbols, count, Bracket(s))
        }
      case ((symbols, count, Garbage(s)), c) =>
        c match {
          case '>' => (symbols, count, Bracket(s))
          case '!' => (symbols, count, GarbageIgnore(s))
          case _ => (symbols, count + 1, Garbage(s))
        }
      case ((symbols, count, GarbageIgnore(s)), _) => (symbols, count, Garbage(s))
    }
  }

}

sealed trait Symbol

// Inside a bracket group {}
case class Bracket(score: Int) extends Symbol

// Inside a garbage group <>
case class Garbage(score: Int) extends Symbol

// Inside a garbage group and also the previous character was !
case class GarbageIgnore(score: Int) extends Symbol
