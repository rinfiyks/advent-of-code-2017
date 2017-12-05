package puzzles

import util.Util
import collection.mutable

object Day3 extends App {

  val input = Util.readInput("day3.txt").next().toInt
  println(solve1(input))
  println(solve2(input))

  // O(1) formulaic solution using the fact that the top-left and bottom-right corners are the sequence of square numbers
  def solve1(input: Int): Int = {
    val s = math.sqrt(input).toInt
    val d = input - s.toInt * s.toInt
    if (d == 0) return s - 1
    val botDiff = input - s * s
    if (botDiff <= s / 2) return s - botDiff + 1
    val topDiff = (s + 1) * (s + 1) - input
    if (topDiff <= s / 2) return s - topDiff
    val botParity = (s + 1) % 2
    val topParity = s % 2
    if (botDiff < topDiff) botDiff - botParity
    else topDiff + topParity
  }

  case class Point(x: Int, y: Int)

  case class Square(p: Point, value: Int)

  // O(n) iterative solution because I have better things to do
  def solve2(input: Int): Int = {
    val points = mutable.Set(Square(Point(0, 0), 1))
    var currentPoint = Point(1, 0)
    var remainingPointsInDirection = 1
    var directionSize = 1
    var xVelocity = 0
    var yVelocity = 1

    while (true) {
      val s = sumAdjacentPoints(points, currentPoint)
      if (s > input) return s
      points.add(Square(currentPoint, s))
      currentPoint = Point(currentPoint.x + xVelocity, currentPoint.y + yVelocity)
      remainingPointsInDirection -= 1
      if (remainingPointsInDirection == 0) {
        if (yVelocity == 0) {
          if (xVelocity == 1) {
            yVelocity = 1
          } else {
            yVelocity = -1
          }
          xVelocity = 0
        } else {
          if (yVelocity == 1) {
            xVelocity = -1
          } else {
            xVelocity = 1
          }
          yVelocity = 0
          directionSize += 1
        }
        remainingPointsInDirection = directionSize
      }
    }
    0
  }

  private def sumAdjacentPoints(squares: mutable.Set[Square], p: Point) = {
    val adjacentPoints = for {
      i <- -1 to 1
      j <- -1 to 1
      value <- squares.find(s => s.p.x == p.x + i && s.p.y == p.y + j)
    } yield value
    adjacentPoints.map(_.value).sum
  }

}
