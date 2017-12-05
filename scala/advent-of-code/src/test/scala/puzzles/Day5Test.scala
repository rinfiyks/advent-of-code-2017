package puzzles

import org.scalatest.FunSuite

class Day5Test extends FunSuite {

  test("In this example, the exit is reached in 5 steps.") {
    assert(Day5.solve1(Vector(0, 3, 0, 1, -3)) === 5)
  }

  test("Using this rule with the above example, the process now takes 10 steps, and the offset values after finding the exit are left as 2 3 2 3 -1.") {
    assert(Day5.solve2(Vector(0, 3, 0, 1, -3)) === 10)
  }

}
