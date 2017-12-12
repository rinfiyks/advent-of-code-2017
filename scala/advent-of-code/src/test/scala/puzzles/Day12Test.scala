package puzzles

import org.scalatest.FunSuite

class Day12Test extends FunSuite {

  private val input =
    """
      |0 <-> 2
      |1 <-> 1
      |2 <-> 0, 3, 4
      |3 <-> 2, 4
      |4 <-> 2, 3, 6
      |5 <-> 6
      |6 <-> 4, 5
    """.trim.stripMargin.split("\\n").toList

  test("Therefore, a total of 6 programs are in this group; all but program 1, which has a pipe that connects it to itself.") {
    assert(Day12.solve1(input) === 6)
  }

  test("In the example above, there were 2 groups: one consisting of programs 0,2,3,4,5,6, and the other consisting solely of program 1.") {
    assert(Day12.solve2(input) === 2)
  }

}
