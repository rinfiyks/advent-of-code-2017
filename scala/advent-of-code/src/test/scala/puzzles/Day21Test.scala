package puzzles

import org.scalatest.FunSuite

class Day21Test extends FunSuite {

  private val input =
    """
      |../.# => ##./#../...
      |.#./..#/### => #..#/..../..../#..#
    """.trim.stripMargin.split("\\n").toList

  test("Thus, after 2 iterations, the grid contains 12 pixels that are on.") {
    assert(Day21.solve1(input, 2) === 12)
  }

}
