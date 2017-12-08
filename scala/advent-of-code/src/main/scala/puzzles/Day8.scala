package puzzles

import util.Util

import scala.collection.immutable.Seq

object Day8 extends App {

  val input = Util.readInput("day8.txt").toList
  println(solve1(input))
  println(solve2(input))

  def solve1(input: Seq[String]) = {
    input.foldLeft(Map.empty[String, Int].withDefaultValue(0)) {
      case (registers, in) =>
        processInstruction(registers, in)
    }.maxBy(_._2)._2
  }

  def solve2(input: Seq[String]) = {
    input.scanLeft(Map.empty[String, Int].withDefaultValue(0)) {
      case (registers, in) =>
        processInstruction(registers, in)
    }.flatten.maxBy(_._2)._2
  }

  case class Instruction(register: String, op: String, opVal: Int, regToCheck: String, comparison: String, comparisonVal: Int)

  private def processInstruction(registers: Map[String, Int], in: String): Map[String, Int] = {
    val instruction = parseInstruction(in)
    if (eval(instruction, registers(instruction.regToCheck))) {
      registers.updated(instruction.register, newVal(instruction, registers(instruction.register)))
    }
    else registers
  }

  private def parseInstruction(instruction: String): Instruction = {
    val s = instruction.split(" ")
    Instruction(s.head, s(1), s(2).toInt, s(4), s(5), s(6).toInt)
  }

  private def eval(instruction: Instruction, regVal: Int) = {
    instruction.comparison match {
      case "<" => regVal < instruction.comparisonVal
      case "<=" => regVal <= instruction.comparisonVal
      case ">" => regVal > instruction.comparisonVal
      case ">=" => regVal >= instruction.comparisonVal
      case "==" => regVal == instruction.comparisonVal
      case "!=" => regVal != instruction.comparisonVal
    }
  }

  private def newVal(instruction: Instruction, currentVal: Int) = {
    instruction.op match {
      case "dec" => currentVal - instruction.opVal
      case "inc" => currentVal + instruction.opVal
    }
  }

}
