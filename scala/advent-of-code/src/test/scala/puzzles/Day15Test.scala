package puzzles

import org.scalatest.FunSuite

class Day15Test extends FunSuite {

  test("Because of this one match, after processing these five pairs, the judge would have added only 1 to its total.") {
    assert(Day15.solve1(65, 8921, 5) === 1)
  }

  test("Using the values from the example above, after five million pairs, the judge would eventually find a total of 309 pairs that match in their lowest 16 bits.") {
    assert(Day15.solve2(65, 8921, 5000000) === 309)
  }

}
