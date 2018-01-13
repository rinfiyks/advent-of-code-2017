package puzzles

import util.Util

import collection.immutable.Seq

object Day20 extends App {

  val input = Util.readInput("day20.txt").toList

  println(solve1(input))
  println(solve2(input))

  def solve1(input: Seq[String]) = {
    val particles = parseInput(input)
    val lowestAccel = particles.minBy {
      p =>
        math.abs(p.acceleration.x) + math.abs(p.acceleration.y) + math.abs(p.acceleration.z)
    }
    lowestAccel.id
  }

  def solve2(input: Seq[String]) = {
    val initialParticles = parseInput(input)

    // Not guaranteed to work for all input, may need to increase range
    (0 to 100).foldLeft(initialParticles) {
      case (particles, _) =>
        removeCollisions(particles.map(_.update))
    }.length
  }

  private def removeCollisions(particles: Seq[Particle]): Seq[Particle] = {
    val positions = particles.map(_.position)
    particles.filter(p => positions.count(_ == p.position) < 2)
  }

  private def parseInput(strings: Seq[String]) = {
    strings.zipWithIndex.map {
      case (s, i) =>
        val split = s.split(", ")
        Particle(i, toVector3(split(0)), toVector3(split(1)), toVector3(split(2)))
    }
  }

  private def toVector3(s: String): Vector3 = {
    val i = s.drop(3).dropRight(1).split(",").map(_.toInt)
    Vector3(i(0), i(1), i(2))
  }

}

case class Vector3(x: Int, y: Int, z: Int) {

  def +(other: Vector3): Vector3 = {
    Vector3(x + other.x, y + other.y, z + other.z)
  }

}

case class Particle(id: Int, position: Vector3, velocity: Vector3, acceleration: Vector3) {

  def update: Particle = {
    val newVelocity = velocity + acceleration
    val newPosition = position + newVelocity
    Particle(id, newPosition, newVelocity, acceleration)
  }

}
