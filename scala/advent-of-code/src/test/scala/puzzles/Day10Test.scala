package puzzles

import org.scalatest.FunSuite

class Day10Test extends FunSuite {

  test("In this example, the first two numbers in the list end up being 3 and 4; to check the process, you can multiply them together to produce 12.") {
    assert(Day10.solve1("3,4,1,5", 5) === 12)
  }

  test("The empty string becomes a2582a3a0e66e6e86e3812dcb672a272.") {
    assert(Day10.solve2("", 256) === "a2582a3a0e66e6e86e3812dcb672a272")
  }
  test("AoC 2017 becomes 33efeb34ea91902bb2f59c9920caa6cd.") {
    assert(Day10.solve2("AoC 2017", 256) === "33efeb34ea91902bb2f59c9920caa6cd")
  }
  test("1,2,3 becomes 3efbe78a8d82f29979031a4aa0b16a9d.") {
    assert(Day10.solve2("1,2,3", 256) === "3efbe78a8d82f29979031a4aa0b16a9d")
  }
  test("1,2,4 becomes 63960835bcdc130f0b66d7ff4f6a5a8e.") {
    assert(Day10.solve2("1,2,4", 256) === "63960835bcdc130f0b66d7ff4f6a5a8e")
  }

}
