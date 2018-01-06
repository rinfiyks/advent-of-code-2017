package puzzles

import org.scalatest.FunSuite

class Day19Test extends FunSuite {

  private val testInput =
    """
      |     |
      |     |  +--+
      |     A  |  C
      | F---|----E|--+
      |     |  |  |  D
      |     +B-+  +--+
    """.trim.stripMargin.split("\\n").toList

  test("Following the path to the end, the letters it sees on its path are ABCDEF.") {
    assert(Day19.solve1(testInput) === "ABCDEF")
  }

  test("This would result in a total of 38 steps.") {
    assert(Day19.solve2(testInput) === 38)
  }

}
