package puzzles

import org.scalatest.FunSuite

class Day1Test extends FunSuite {

  test("1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second digit and the third digit (2) matches the fourth digit.") {
    assert(Day1.solve1("1122") === 3)
  }
  test("1111 produces 4 because each digit (all 1) matches the next.") {
    assert(Day1.solve1("1111") === 4)
  }
  test("1234 produces 0 because no digit matches the next.") {
    assert(Day1.solve1("1234") === 0)
  }
  test("91212129 produces 9 because the only digit that matches the next one is the last digit, 9.") {
    assert(Day1.solve1("91212129") === 9)
  }
  test("1212 produces 6: the list contains 4 items, and all four digits match the digit 2 items ahead.") {
    assert(Day1.solve2("1212") === 6)
  }
  test("1221 produces 0, because every comparison is between a 1 and a 2.") {
    assert(Day1.solve2("1221") === 0)
  }
  test("123425 produces 4, because both 2s match each other, but no other digit has a match.") {
    assert(Day1.solve2("123425") === 4)
  }
  test("123123 produces 12.") {
    assert(Day1.solve2("123123") === 12)
  }
  test("12131415 produces 4.") {
    assert(Day1.solve2("12131415") === 4)
  }

}
