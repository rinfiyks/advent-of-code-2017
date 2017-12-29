package puzzles

import org.scalatest.FunSuite

class Day18Test extends FunSuite {

  private val input1 =
    """
      |set a 1
      |add a 2
      |mul a a
      |mod a 5
      |snd a
      |set a 0
      |rcv a
      |jgz a -1
      |set a 1
      |jgz a -2
    """.trim.stripMargin.split("\\n").toList

  private val input2 =
    """snd 1
      |snd 2
      |snd p
      |rcv a
      |rcv b
      |rcv c
      |rcv d
    """.trim.stripMargin.split("\\n").toList

  test("At the time the recover operation is executed, the frequency of the last sound played is 4.") {
    assert(Day18.solve1(input1) === 4)
  }

  test("Finally, both programs try to rcv a fourth time, but no data is waiting for either of them, and they reach a deadlock. When this happens, both programs terminate.") {
    assert(Day18.solve2(input2) === 3)
  }

}
