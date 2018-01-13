package puzzles

import org.scalatest.FunSuite

class Day20Test extends FunSuite {

  private val input1 =
    """
      |p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
      |p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>
    """.trim.stripMargin.split("\\n").toList

  private val input2 =
    """
      |p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>
      |p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>
      |p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>
      |p=<3,0,0>, v=<1,0,0>, a=<0,0,0>
    """.trim.stripMargin.split("\\n").toList

  test("At this point, particle 1 will never be closer to <0,0,0> than particle 0, and so, in the long run, particle 0 will stay closest.") {
    assert(Day20.solve1(input1) === 0)
  }

  test("In this example, particles 0, 1, and 2 are simultaneously destroyed at the time and place marked X. On the next tick, particle 3 passes through unharmed.") {
    assert(Day20.solve2(input2) === 1)
  }

}
