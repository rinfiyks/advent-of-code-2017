package puzzles

import org.scalatest.FunSuite

class Day6Test extends FunSuite {

  test("The infinite loop is detected after the fifth block redistribution cycle, and so the answer in this example is 5.") {
    assert(Day6.solve1(List(0, 2, 7, 0)) === 5)
  }

  test("In the example above, 2 4 1 2 is seen again after four cycles, and so the answer in that example would be 4.") {
    assert(Day6.solve2(List(0, 2, 7, 0)) === 4)
  }

}
