package puzzles

import util.Util

object Day22 extends App {

  val input = Util.readInput("day22.txt").toVector

  println(solve1(input, 10000))
  println(solve2(input, 10000000))

  def solve1(input: Vector[String], bursts: Int) = {
    val virusState = VirusState(Position(input.length / 2, input.length / 2), U)
    recurse(BasicVirus(State(virusState, input)), 0, bursts)
  }

  def solve2(input: Vector[String], bursts: Int) = {
    val virusState = VirusState(Position(input.length / 2, input.length / 2), U)
    recurse(EnhancedVirus(State(virusState, input)), 0, bursts)
  }

  private def recurse(virus: Virus, infections: Int, bursts: Int): Int = {
    bursts match {
      case 0 => infections
      case _ =>
        val nextInfections = infections + (if (virus.willInfect) 1 else 0)
        recurse(virus.nextState, nextInfections, bursts - 1)
    }
  }

  case class VirusState(pos: Position, dir: Direction) {

    def move(d: Direction): VirusState = {
      val nextPos = d match {
        case U => pos.copy(y = pos.y - 1)
        case D => pos.copy(y = pos.y + 1)
        case L => pos.copy(x = pos.x - 1)
        case R => pos.copy(x = pos.x + 1)
      }
      VirusState(nextPos, d)
    }

  }

  sealed trait Virus {
    def willInfect: Boolean

    def nextState: Virus
  }

  case class BasicVirus(state: State) extends Virus {

    def willInfect: Boolean = state.currentNode == '.'

    def nextState: Virus = BasicVirus(State(state.virus.move(nextDir), nextGrid).enlargeGridIfNecessary)

    private def nextDir = {
      state.currentNode match {
        case '.' =>
          state.virus.dir match {
            case U => L
            case L => D
            case D => R
            case R => U
          }
        case '#' =>
          state.virus.dir match {
            case U => R
            case R => D
            case D => L
            case L => U
          }
      }
    }

    private def nextGrid = {
      val newNode = state.currentNode match {
        case '.' => '#'
        case '#' => '.'
      }
      state.updateCurrentNodeWith(newNode)
    }

  }

  case class EnhancedVirus(state: State) extends Virus {

    def willInfect: Boolean = state.currentNode == 'W'

    def nextState: Virus = EnhancedVirus(State(state.virus.move(nextDir), nextGrid).enlargeGridIfNecessary)

    private def nextDir = {
      state.currentNode match {
        case '.' =>
          state.virus.dir match {
            case U => L
            case L => D
            case D => R
            case R => U
          }
        case '#' =>
          state.virus.dir match {
            case U => R
            case R => D
            case D => L
            case L => U
          }
        case 'F' =>
          state.virus.dir match {
            case U => D
            case R => L
            case D => U
            case L => R
          }
        case _ => state.virus.dir
      }
    }

    private def nextGrid = {
      val newNode = state.currentNode match {
        case '.' => 'W'
        case 'W' => '#'
        case '#' => 'F'
        case 'F' => '.'
      }
      state.updateCurrentNodeWith(newNode)
    }

  }

  case class State(virus: VirusState, grid: Vector[String]) {

    def currentNode: Char = grid(virus.pos.y)(virus.pos.x)

    def updateCurrentNodeWith(newNode: Char): Vector[String] =
      grid.updated(virus.pos.y, grid(virus.pos.y).updated(virus.pos.x, newNode))

    def enlargeGridIfNecessary: State = {
      virus.pos match {
        case Position(x, y) if x == 0 =>
          val newVirus = VirusState(Position(1, virus.pos.y), virus.dir)
          val newGrid = grid.map(l => "." + l)
          State(newVirus, newGrid)

        case Position(x, y) if y == 0 =>
          val newVirus = VirusState(Position(virus.pos.x, 1), virus.dir)
          val newGrid = grid.head.map(_ => '.') +: grid
          State(newVirus, newGrid)

        case Position(x, y) if x == grid.head.length - 1 =>
          val newGrid = grid.map(l => l + ".")
          State(virus, newGrid)

        case Position(x, y) if y == grid.length - 1 =>
          val newGrid = grid :+ grid.head.map(_ => '.')
          State(virus, newGrid)

        case _ => this
      }
    }

  }

}
