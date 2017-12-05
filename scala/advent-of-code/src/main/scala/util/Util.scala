package util

object Util {

  def readInput(input: String): Iterator[String] = {
    io.Source.fromResource(s"puzzles/input/$input").getLines()
  }

}
