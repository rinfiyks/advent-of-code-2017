package puzzles

import org.scalatest.FunSuite

class Day8Test extends FunSuite {

  private val input =
    """
      |b inc 5 if a > 1
      |a inc 1 if b < 5
      |c dec -10 if a >= 1
      |c inc -20 if c == 10
    """.trim.stripMargin.split("\\n").toList

  test("After this process, the largest value in any register is 1.") {
    assert(Day8.solve1(input) === 1)
  }

  test("For example, in the above instructions, the highest value ever held was 10 (in register c after the third instruction was evaluated).") {
    assert(Day8.solve2(input) === 10)
  }

}
