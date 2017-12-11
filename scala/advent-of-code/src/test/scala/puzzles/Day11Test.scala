package puzzles

import org.scalatest.FunSuite

class Day11Test extends FunSuite {

  test("ne,ne,ne is 3 steps away.") {
    assert(Day11.solve1("ne,ne,ne") === 3)
  }
  test("ne,ne,sw,sw is 0 steps away (back where you started).") {
    assert(Day11.solve1("ne,ne,sw,sw") === 0)
  }
  test("ne,ne,s,s is 2 steps away (se,se).") {
    assert(Day11.solve1("ne,ne,s,s") === 2)
  }
  test("se,sw,se,sw,sw is 3 steps away (s,s,sw).") {
    assert(Day11.solve1("se,sw,se,sw,sw") === 3)
  }

}
