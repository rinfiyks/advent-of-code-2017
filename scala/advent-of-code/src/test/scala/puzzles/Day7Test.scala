package puzzles

import org.scalatest.FunSuite

class Day7Test extends FunSuite {

  val input =
    """
      |pbga (66)
      |xhth (57)
      |ebii (61)
      |havc (66)
      |ktlj (57)
      |fwft (72) -> ktlj, cntj, xhth
      |qoyq (66)
      |padx (45) -> pbga, havc, qoyq
      |tknk (41) -> ugml, padx, fwft
      |jptl (61)
      |ugml (68) -> gyxo, ebii, jptl
      |gyxo (61)
      |cntj (57)
    """.stripMargin.trim.split("\\n").toList

  test("Before you're ready to help them, you need to make sure your information is correct. What is the name of the bottom program?") {
    assert(Day7.solve1(input) === "tknk")
  }

  test("ugml needs to be 8 units lighter for its stack to weigh 243 and keep the towers balanced. If this change were made, its weight would be 60.") {
    assert(Day7.solve2(input) === 60)
  }

}
