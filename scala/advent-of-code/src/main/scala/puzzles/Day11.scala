package puzzles

import util.Util

object Day11 extends App {

  val input = Util.readInput("day11.txt").next()
  println(solve1(input))
  println(solve2(input))

  def solve1(input: String) = {
    val directions = parseInput(input)
    directions.foldLeft(Hex(0, 0)) {
      case (hex, direction) => hex.move(direction)
    }.distanceFromCentre
  }

  def solve2(input: String) = {
    val directions = parseInput(input)
    directions.scanLeft(Hex(0, 0)) {
      case (hex, direction) => hex.move(direction)
    }.map(_.distanceFromCentre).max
  }

  private def parseInput(input: String) = {
    input.split(",").map {
      case "nw" => NW
      case "n" => N
      case "ne" => NE
      case "sw" => SW
      case "s" => S
      case "se" => SE
    }.toList
  }

  case class Hex(col: Int, row: Int) {

    def move(direction: HexDirection): Hex = {
      direction match {
        case NW => Hex(col - 1, row - (col & 1))
        case N => Hex(col, row - 1)
        case NE => Hex(col + 1, row - (col & 1))
        case SW => Hex(col - 1, row + (1 - col & 1))
        case S => Hex(col, row + 1)
        case SE => Hex(col + 1, row + (1 - col & 1))
      }
    }

    def distanceFromCentre: Int = {
      val x = col
      val z = row - (col + (col & 1)) / 2
      val y = -x - z
      (math.abs(x) + math.abs(y) + math.abs(z)) / 2
    }

  }

}

sealed trait HexDirection

case object NW extends HexDirection

case object N extends HexDirection

case object NE extends HexDirection

case object SW extends HexDirection

case object S extends HexDirection

case object SE extends HexDirection
