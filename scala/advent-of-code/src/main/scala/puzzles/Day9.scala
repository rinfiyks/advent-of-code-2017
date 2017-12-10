package puzzles

import util.Util

object Day9 extends App {

  val input = Util.readInput("day9.txt").next()
  println(solve1(input))
  println(solve2(input))

  def solve1(input: String): Int = {
    input.foldLeft[(List[Symbol], Symbol)]((List.empty[Symbol], Bracket(0))) {
      case ((symbols, Bracket(s)), c) =>
        c match {
          case '}' => (Bracket(s) +: symbols, Bracket(s - 1))
          case '{' => (symbols, Bracket(s + 1))
          case '<' => (symbols, Garbage(s))
          case _ => (symbols, Bracket(s))
        }
      case ((symbols, Garbage(s)), c) =>
        c match {
          case '>' => (symbols, Bracket(s))
          case '!' => (symbols, GarbageIgnore(s))
          case _ => (symbols, Garbage(s))
        }
      case ((symbols, GarbageIgnore(s)), _) => (symbols, Garbage(s))
    }._1.collect { case s: Bracket => s }.map(_.score).sum
  }

  def solve2(input: String): Int = {
    input.foldLeft[(Int, Symbol)]((0, Bracket(0))) {
      case ((count, Bracket(s)), c) =>
        c match {
          case '}' => (count, Bracket(s - 1))
          case '{' => (count, Bracket(s + 1))
          case '<' => (count, Garbage(s))
          case _ => (count, Bracket(s))
        }
      case ((count, Garbage(s)), c) =>
        c match {
          case '>' => (count, Bracket(s))
          case '!' => (count, GarbageIgnore(s))
          case _ => (count + 1, Garbage(s))
        }
      case ((count, GarbageIgnore(s)), _) => (count, Garbage(s))
    }._1
  }

}

sealed trait Symbol

// Inside a bracket group {}
case class Bracket(score: Int) extends Symbol

// Inside a garbage group <>
case class Garbage(score: Int) extends Symbol

// Inside a garbage group and also the previous character was !
case class GarbageIgnore(score: Int) extends Symbol
