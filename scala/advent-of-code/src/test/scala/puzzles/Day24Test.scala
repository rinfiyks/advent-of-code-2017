package puzzles

import org.scalatest.FunSuite

class Day24Test extends FunSuite {

  private val input =
    """
      |0/2
      |2/2
      |2/3
      |3/4
      |3/5
      |0/1
      |10/1
      |9/10
    """.trim.stripMargin.split("\\n").toList

  test("Of these bridges, the strongest one is 0/1--10/1--9/10; it has a strength of 0+1 + 1+10 + 10+9 = 31.") {
    assert(Day24.solve1(input) === 31)
  }

  test("Of them, the one which uses the 3/5 component is stronger; its strength is 0+2 + 2+2 + 2+3 + 3+5 = 19.") {
    assert(Day24.solve2(input) === 19)
  }

}
