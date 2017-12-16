package puzzles

import org.scalatest.FunSuite

class Day13Test extends FunSuite {

  private val input =
    """
      |0: 3
      |1: 2
      |4: 4
      |6: 4
    """.trim.stripMargin.split("\\n").toList

  test(" In the example above, the trip severity is 0*3 + 6*4 = 24.") {
    assert(Day13.solve1(input) === 24)
  }

  test("Because all smaller delays would get you caught, the fewest number of picoseconds you would need to delay to get through safely is 10.") {
    assert(Day13.solve2(input) === 10)
  }

}
