package puzzles

import org.scalatest.FunSuite

class Day4Test extends FunSuite {

  test("aa bb cc dd ee is valid.") {
    assert(Day4.solve1(List("aa bb cc dd ee")) === 1)
  }
  test("aa bb cc dd aa is not valid - the word aa appears more than once.") {
    assert(Day4.solve1(List("aa bb cc dd aa")) === 0)
  }
  test("aa bb cc dd aaa is valid - aa and aaa count as different words.") {
    assert(Day4.solve1(List("aa bb cc dd aaa")) === 1)
  }

  test("abcde fghij is a valid passphrase.") {
    assert(Day4.solve2(List("abcde fghij")) === 1)
  }
  test("abcde xyz ecdab is not valid - the letters from the third word can be rearranged to form the first word.") {
    assert(Day4.solve2(List("abcde xyz ecdab")) === 0)
  }
  test("a ab abc abd abf abj is a valid passphrase, because all letters need to be used when forming another word.") {
    assert(Day4.solve2(List("a ab abc abd abf abj")) === 1)
  }
  test("iiii oiii ooii oooi oooo is valid.") {
    assert(Day4.solve2(List("iiii oiii ooii oooi oooo")) === 1)
  }
  test("oiii ioii iioi iiio is not valid - any of these words can be rearranged to form any other word.") {
    assert(Day4.solve2(List("oiii ioii iioi iiio")) === 0)
  }

}
