package com.github.falvarezb

case class LineNumber(value: Int) extends AnyVal
case class InstructionNumber(value: Int) extends AnyVal:
  def +(other: InstructionNumber): InstructionNumber = InstructionNumber(value + other.value)
  def -(other: InstructionNumber): InstructionNumber = InstructionNumber(value - other.value)
  def ∆+(other: Int): InstructionNumber = InstructionNumber(value + other)
  def ∆-(other: Int): InstructionNumber = InstructionNumber(value - other)
  def ∇+(other: Int): Int = value - other
  def ∇-(other: Int): Int = value - other
case class LineMetadata(tokenizedLine: Array[String], lineNumber: LineNumber):
  val isOpCode = OpCode.values.map(_.toString).contains(this.tokenizedLine(0))
  val isDirective = Directive.values.map(_.toString).contains(this.tokenizedLine(0))
  val isComment = this.tokenizedLine(0)(0) == ';'
case class InstructionMetadata(lineMetadata: LineMetadata, instructionNumber: InstructionNumber)

enum OpCode:
  case ADD, JSR

enum Directive:
  case `.ORIG`, HALT