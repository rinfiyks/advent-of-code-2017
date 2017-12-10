package puzzles

import org.scalatest.FunSuite

class Day9Test extends FunSuite {

  test("{}, score of 1.") {
    assert(Day9.solve1("{}") === 1)
  }
  test("{{{}}}, score of 1 + 2 + 3 = 6.") {
    assert(Day9.solve1("{{{}}}") === 6)
  }
  test("{{},{}}, score of 1 + 2 + 2 = 5.") {
    assert(Day9.solve1("{{},{}}") === 5)
  }
  test("{{{},{},{{}}}}, score of 1 + 2 + 3 + 3 + 3 + 4 = 16.") {
    assert(Day9.solve1("{{{},{},{{}}}}") === 16)
  }
  test("{<a>,<a>,<a>,<a>}, score of 1.") {
    assert(Day9.solve1("{<a>,<a>,<a>,<a>}") === 1)
  }
  test("{{<ab>},{<ab>},{<ab>},{<ab>}}, score of 1 + 2 + 2 + 2 + 2 = 9.") {
    assert(Day9.solve1("{{<ab>},{<ab>},{<ab>},{<ab>}}") === 9)
  }
  test("{{<!!>},{<!!>},{<!!>},{<!!>}}, score of 1 + 2 + 2 + 2 + 2 = 9.") {
    assert(Day9.solve1("{{<!!>},{<!!>},{<!!>},{<!!>}}") === 9)
  }
  test("{{<a!>},{<a!>},{<a!>},{<ab>}}, score of 1 + 2 = 3.") {
    assert(Day9.solve1("{{<a!>},{<a!>},{<a!>},{<ab>}}") === 3)
  }

  test("<>, 0 characters.") {
    assert(Day9.solve2("<>") === 0)
  }
  test("<random characters>, 17 characters.") {
    assert(Day9.solve2("<random characters>") === 17)
  }
  test("<<<<>, 3 characters.") {
    assert(Day9.solve2("<<<<>") === 3)
  }
  test("<{!>}>, 2 characters.") {
    assert(Day9.solve2("<{!>}>") === 2)
  }
  test("<!!>, 0 characters.") {
    assert(Day9.solve2("<!!>") === 0)
  }
  test("<!!!>>, 0 characters.") {
    assert(Day9.solve2("<!!!>>") === 0)
  }
  test("<{o\"i!a,<{i<a>, 10 characters."){
    assert(Day9.solve2("<{o\"i!a,<{i<a>") === 10)
  }

}
