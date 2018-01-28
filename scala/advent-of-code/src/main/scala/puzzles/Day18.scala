package puzzles

import util.Util

import collection.immutable.{Queue, Seq}

object Day18 extends App {

  val input = Util.readInput("day18.txt").toList
  println(solve1(input))
  println(solve2(input))

  def solve1(input: Seq[String]) = {
    val initialRegisters = setupInitialRegisters(input)
    recurse1(ProgramState(input, 0, initialRegisters), None).get
  }

  def solve2(input: Seq[String]) = {
    val initialRegisters1 = setupInitialRegisters(input)
    val initialRegisters2 = initialRegisters1.updated("p", 1l)
    val program1State = ProgramState(input, 0, initialRegisters1, Queue.empty)
    val program2State = ProgramState(input, 0, initialRegisters2, Queue.empty)

    recurse2(List(program1State, program2State), 0)
  }

  @annotation.tailrec
  private def recurse1(state: ProgramState, lastSound: Option[Long]): Option[Long] = {
    val nextState = calculateNextState(state)

    val nextSound = state.instructionType match {
      case "snd" => Some(state.registers(state.X))
      case _ => lastSound
    }

    if (state.instructionType == "rcv" && state.XVal != 0) nextSound
    else recurse1(nextState, nextSound)
  }

  @annotation.tailrec
  private def recurse2(programs: List[ProgramState], sendCount: Int): Long = {
    programs.find(p => p.instructionType != "rcv" || p.rcvQueue.nonEmpty) match {
      case Some(p) =>
        p.instructionType match {
          case "snd" =>
            val (updatedPrograms, nextSendCount) = send(p, programs, sendCount)
            recurse2(updatedPrograms, nextSendCount)
          case "rcv" =>
            val updatedPrograms = receive(p, programs)
            recurse2(updatedPrograms, sendCount)
          case _ =>
            recurse2(programs.updated(programs.indexOf(p), calculateNextState(p)), sendCount)
        }
      case None => sendCount
    }
  }

  private def send(p: ProgramState, programs: List[ProgramState], sendCount: Int) = {
    val nextState = calculateNextState(p)
    val valueToSend = p.XVal
    val updatedPrograms = programs.map {
      case q if p != q => q.copy(rcvQueue = q.rcvQueue :+ valueToSend)
      case _ => nextState
    }
    val nextSendCount = if (programs.indexOf(p) == 1) sendCount + 1 else sendCount
    (updatedPrograms, nextSendCount)
  }

  private def receive(p: ProgramState, programs: List[ProgramState]) = {
    val nextState = calculateNextState(p)
    val updatedPrograms: List[ProgramState] = programs.map {
      case q if p == q =>
        val (valueReceived, nextQueue) = q.rcvQueue.dequeue
        val updatedRegisters = q.registers.updated(q.X, valueReceived)
        nextState.copy(registers = updatedRegisters, rcvQueue = nextQueue)
      case q => q
    }
    updatedPrograms
  }

  def calculateNextState(state: ProgramState): ProgramState = {
    val op: Option[(Long, Long) => Long] = state.instructionType match {
      case "set" => Some((_, X) => X)
      case "add" => Some(_ + _)
      case "sub" => Some(_ - _)
      case "mul" => Some(_ * _)
      case "mod" => Some(_ % _)
      case _ => None
    }

    val updatedRegisters = op match {
      case Some(f) =>
        state.registers.updated(state.X, f(state.XVal, state.Y))
      case None => state.registers
    }

    val nextPointer: Long = state.instructionType match {
      case "jgz" =>
        if (state.XVal > 0) state.pointer + state.Y else state.pointer + 1
      case "jnz" =>
        if (state.XVal != 0) state.pointer + state.Y else state.pointer + 1
      case _ => state.pointer + 1
    }

    state.copy(pointer = nextPointer.toInt, registers = updatedRegisters)
  }

  def setupInitialRegisters(input: Seq[String]) = {
    input.map(_.split(" ")(1))
      .filter(_.head.isLetter)
      .map(_ -> 0l).toMap
  }

}

case class ProgramState(input: Seq[String], pointer: Int, registers: Map[String, Long], rcvQueue: Queue[Long]) {
  lazy val split: Array[String] = input(pointer).split(" ")
  lazy val instructionType: String = split(0)
  lazy val X: String = split(1)
  lazy val XVal: Long = if (X.head.isLetter) registers(X) else X.toLong
  lazy val Y: Long = if (split(2).head.isLetter) registers(split(2)) else split(2).toLong
}

object ProgramState {
  def apply(input: Seq[String], pointer: Int, registers: Map[String, Long]) =
    new ProgramState(input, pointer, registers, Queue.empty)
}
