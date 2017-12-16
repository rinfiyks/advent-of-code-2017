package puzzles

import org.scalatest.FunSuite

class Day14Test extends FunSuite {

  test("In this example, 8108 squares are used across the entire 128x128 grid.") {
    assert(Day14.solve1("flqrgnkx") == 8108)
  }

  test("In total, in this example, 1242 regions are present.") {
    assert(Day14.solve2("flqrgnkx") == 1242)
  }

}
