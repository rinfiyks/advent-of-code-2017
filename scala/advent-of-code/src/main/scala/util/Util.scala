package util

object Util {

  def readInput(input: String): Seq[String] = {
    io.Source.fromResource(s"puzzles/input/$input").getLines().toList
  }

}
