package puzzles

import org.scalatest.FunSuite

class Day3Test extends FunSuite {

  test("Data from square 1 is carried 0 steps, since it's at the access port.") {
    assert(Day3.solve1(1) === 0)
  }
  test("Data from square 12 is carried 3 steps, such as: down, left, left.") {
    assert(Day3.solve1(12) === 3)
  }
  test("Data from square 23 is carried only 2 steps: up twice.") {
    assert(Day3.solve1(23) === 2)
  }
  test("Data from square 1024 must be carried 31 steps.") {
    assert(Day3.solve1(1024) === 31)
  }

}
