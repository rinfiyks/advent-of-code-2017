package puzzles

import org.scalatest.FunSuite

class Day22Test extends FunSuite {

  private val input =
    """
      |..#
      |#..
      |...
    """.trim.stripMargin.split("\\n").toVector

  test("After the above actions, a total of 7 bursts of activity had taken place. Of them, 5 bursts of activity caused an infection.") {
    assert(Day22.solve1(input, 7) === 5)
  }
  test("AfterBy this time, 41 bursts of activity caused an infection (though most of those nodes have since been cleaned).") {
    assert(Day22.solve1(input, 70) === 41)
  }
  test("After a total of 10000 bursts of activity, 5587 bursts will have caused an infection.") {
    assert(Day22.solve1(input, 10000) === 5587)
  }

  test("Of the first 100 bursts, 26 will result in infection.") {
    assert(Day22.solve2(input, 100) === 26)
  }
  test("Unfortunately, another feature of this evolved virus is speed; of the first 10000000 bursts, 2511944 will result in infection.") {
    assert(Day22.solve2(input, 10000000) === 2511944)
  }

}
