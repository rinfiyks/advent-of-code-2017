package puzzles

import org.scalatest.FunSuite

class Day2Test extends FunSuite {

  private val test1Input = Day2.parseInput(List("5 1 9 5", "7 5 3", "2 4 6 8"))

  test("The first row's largest and smallest values are 9 and 1, and their difference is 8.") {
    assert(Day2.rowDifference(test1Input(0)) === 8)
  }
  test("The second row's largest and smallest values are 7 and 3, and their difference is 4.") {
    assert(Day2.rowDifference(test1Input(1)) === 4)
  }
  test("The third row's difference is 6.") {
    assert(Day2.rowDifference(test1Input(2)) === 6)
  }
  test("In this example, the spreadsheet's checksum would be 8 + 4 + 6 = 18.") {
    assert(Day2.solve1(test1Input) === 18)
  }

  private val test2Input = Day2.parseInput(List("5 9 2 8", "9 4 7 3", "3 8 6 5"))

  test("In the first row, the only two numbers that evenly divide are 8 and 2; the result of this division is 4.") {
    assert(Day2.divisionOfTheTwoThatAreDivisible(test2Input(0)) === 4)
  }
  test("In the second row, the two numbers are 9 and 3; the result is 3.") {
    assert(Day2.divisionOfTheTwoThatAreDivisible(test2Input(1)) === 3)
  }
  test("In the third row, the result is 2.") {
    assert(Day2.divisionOfTheTwoThatAreDivisible(test2Input(2)) === 2)
  }
  test("In this example, the sum of the results would be 4 + 3 + 2 = 9.") {
    assert(Day2.solve2(test2Input) === 9)
  }

}
