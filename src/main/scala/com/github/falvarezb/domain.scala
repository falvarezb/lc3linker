package com.github.falvarezb

case class LineNumber(value: Int) extends AnyVal
case class InstructionMemoryAddress(value: Int) extends AnyVal:
  def +(other: InstructionMemoryAddress): InstructionMemoryAddress = InstructionMemoryAddress(value + other.value)
  def -(other: InstructionMemoryAddress): InstructionMemoryAddress = InstructionMemoryAddress(value - other.value)
  def ∆+(other: Int): InstructionMemoryAddress = InstructionMemoryAddress(value + other)
  def ∆-(other: Int): InstructionMemoryAddress = InstructionMemoryAddress(value - other)
  def ∇+(other: Int): Int = value - other
  def ∇-(other: Int): Int = value - other
case class LineMetadata(line: String, tokenizedLine: List[String], lineNumber: LineNumber):
  val isOpCode = OpCode.values.map(_.toString).contains(this.tokenizedLine.headOption.getOrElse(""))
  val isDirective = Directive.values.map(_.toString).contains(this.tokenizedLine.headOption.getOrElse(""))
  val isComment = this.tokenizedLine.headOption.exists(_.startsWith(";"))
case class InstructionMetadata(lineMetadata: LineMetadata, instructionMemoryAddress: InstructionMemoryAddress)

enum OpCode:
  case ADD, JSR

enum Directive:
  case `.ORIG`, HALT, `.STRINGZ`, `.BLKW`