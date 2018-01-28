package puzzles

import util.Util

import collection.immutable.Seq

object Day23 extends App {

  val input = Util.readInput("day23.txt").toList

  println(solve1(input))
  println(solve2)

  def solve1(input: Seq[String]) = {
    val initialRegisters = Day18.setupInitialRegisters(input)
    val initialState = ProgramState(input, 0, initialRegisters)
    recurse(List(initialState), 0)
  }

  def solve2 = solve2Version4

  @annotation.tailrec
  private def recurse(states: List[ProgramState], count: Int): Int = {
    val nextCount = if (states.head.instructionType == "mul") {
      count + 1
    } else count
    val nextState = Day18.calculateNextState(states.head)

    if (nextState.pointer >= nextState.input.length) count
    else recurse(nextState +: states, nextCount)
  }

  // translated assembly instructions
  private def solve2Version1: Int = {
    var a, b, c, d, e, f, g, h = 0
    b = 67
    c = 27
    if (a != 0) {
      b *= 100
      b += 100000
      c = b
      c += 17000
    }
    do {
      f = 1
      d = 2
      do {
        e = 2
        do {
          g = d
          g *= e
          g -= b
          if (g == 0) {
            f = 0
          }
          e += 1
          g = e
          g -= b
        } while (g != 0)
        d += 1
        g = d
        g -= b
      } while (g != 0)
      if (f == 0) {
        h += 1
      }
      g = b
      g -= c
      if (g == 0) {
        return h
      }
      b += 17
    } while (true)
    0
  }

  // removed all the g's
  private def solve2Version2: Int = {
    var b, c, d, e, f, h = 0
    b = 106700
    c = 123700
    while (true) {
      f = 1
      d = 2
      do {
        e = 2
        do {
          if (d * e == b) {
            f = 0
          }
          e += 1
        } while (e != b)
        d += 1
      } while (b != d)
      if (f == 0) {
        h += 1
      }
      if (b == c) {
        return h
      }
      b += 17
    }
    0
  }

  // refactored into some loops
  private def solve2Version3: Int = {
    var b, c, f, h = 0
    b = 106700
    c = 123700
    (106700 to 123700 by 17) foreach { b =>
      f = 1
      (2 until b) foreach { d =>
        (2 until b) foreach { e =>
          if (d * e == b) f = 0
        }
      }
      if (f == 0) {
        h += 1
      }
    }
    h
  }

  // jumping out of the system... it's checking for composite numbers
  private def solve2Version4: Int = {
    var b, c, h = 0
    b = 106700
    c = 123700
    (106700 to 123700 by 17) foreach { b =>
      if (isComposite(b)) h += 1
    }
    h
  }

  private def isComposite(n: Int) = (2 until n - 1) exists (n % _ == 0)

}
