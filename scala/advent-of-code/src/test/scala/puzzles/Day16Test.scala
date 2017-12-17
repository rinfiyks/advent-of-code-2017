package puzzles

import org.scalatest.FunSuite

class Day16Test extends FunSuite {

  test("After finishing their dance, the programs end up in order baedc.") {
    assert(Day16.solve1("s1,x3/4,pe/b".split(","), 'a' to 'e') === "baedc")
  }

}
