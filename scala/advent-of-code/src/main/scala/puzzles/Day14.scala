package puzzles

import util.Util

import collection.immutable.Seq

object Day14 extends App {

  val input = Util.readInput("day14.txt").next()
  println(solve1(input))
  println(solve2(input))

  def solve1(input: String): Int = {
    calculateGrid(input).map(_.sum).sum
  }

  private def calculateGrid(input: String): Seq[Seq[Int]] = {
    (0 to 127)
      .map(input + "-" + _)
      .map(row => Day10.solve2(row, 256))
      .map(hex => BigInt(hex, 16).toString(2).map(_.asDigit))
      .map(l => List.fill(128 - l.length)(0) ++ l)
  }

  def solve2(input: String): Int = {
    val grid = calculateGrid(input)

    def getAdjacents(x: Int, y: Int) = {
      for {
        (i, j) <- List((0, 0), (-1, 0), (1, 0), (0, -1), (0, 1)) if
        x + i >= 0 && x + i < 128 &&
          y + j >= 0 && y + j < 128 &&
          grid(x + i)(y + j) == 1
      } yield (x + i, y + j)
    }

    val connections = for {
      x <- 0 to 127
      y <- 0 to 127
      adjacent <- getAdjacents(x, y) if grid(x)(y) == 1
    } yield ((x, y), adjacent)

    val parsedConnections = connections.groupBy(_._1).mapValues {
      _.map {
        case (_, (c, d)) => s"$c-$d"
      }.mkString(", ")
    }.toList.map {
      case ((a, b), s) => s"$a-$b <-> $s"
    }

    Day12.solve2(parsedConnections)
  }
}
