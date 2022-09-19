package com.github.falvarezb

case class LineNumber(value: Int) extends AnyVal
case class InstructionLocation(value: Int) extends AnyVal:
  def +(other: InstructionLocation): InstructionLocation = InstructionLocation(value + other.value)
  def -(other: InstructionLocation): InstructionLocation = InstructionLocation(value - other.value)
  def ∆+(other: Int): InstructionLocation = InstructionLocation(value + other)
  def ∆-(other: Int): InstructionLocation = InstructionLocation(value - other)
  def ∇+(other: Int): Int = value + other
  def ∇-(other: Int): Int = value - other
case class LineMetadata(line: String, tokenizedLine: List[String], lineNumber: LineNumber):
  val isOpCode = OpCode.values.map(_.toString).contains(this.tokenizedLine.headOption.getOrElse(""))
  val isDirective = Directive.values.map(_.toString).contains(this.tokenizedLine.headOption.getOrElse(""))
case class InstructionMetadata(lineMetadata: LineMetadata, instructionLocation: InstructionLocation)

enum OpCode:
  case ADD, JSR

enum Directive:
  case `.ORIG`, HALT, `.STRINGZ`, `.BLKW`


def isComment(str: String) = str.startsWith(";")