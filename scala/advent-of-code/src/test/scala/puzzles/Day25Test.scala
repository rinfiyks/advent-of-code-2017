package puzzles

import org.scalatest.FunSuite

class Day25Test extends FunSuite {

  private val input =
    """
      |Begin in state A.
      |Perform a diagnostic checksum after 6 steps.
      |
      |In state A:
      |  If the current value is 0:
      |    - Write the value 1.
      |    - Move one slot to the right.
      |    - Continue with state B.
      |  If the current value is 1:
      |    - Write the value 0.
      |    - Move one slot to the left.
      |    - Continue with state B.
      |
      |In state B:
      |  If the current value is 0:
      |    - Write the value 1.
      |    - Move one slot to the left.
      |    - Continue with state A.
      |  If the current value is 1:
      |    - Write the value 1.
      |    - Move one slot to the right.
      |    - Continue with state A.
    """.trim.stripMargin.split("\\n").toList

  test("In the above example, the diagnostic checksum is 3.") {
    assert(Day25.solve1(input) === 3)
  }

}
