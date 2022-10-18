package com.github.falvarezb

type SymbolTable = Map[String, InstructionLocation]

case class LineNumber(value: Int) extends AnyVal

case class InstructionLocation(value: Int) extends AnyVal :
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
  case ADD, AND, NOT, JSR, JSRR, JMP, JMPT, LDR, STR, LD, ST, LDI, STI, LEA, BR, BRp, BRz, BRn, BRzp, BRnp, BRnz, BRnzp, TRAP, RET, RTI

enum Directive:
  case `.ORIG`, HALT, `.STRINGZ`, `.BLKW`, `.FILL`, GETC, OUT, PUTS, IN, PUTSP, `.EXTERNAL`

enum ConditionCode(val value: Int):
  case N extends ConditionCode(4) //100
  case Z extends ConditionCode(2) //010
  case P extends ConditionCode(1) //001
  case NZ extends ConditionCode(6) //110
  case NP extends ConditionCode(5) //101
  case ZP extends ConditionCode(3) //011
  case NZP extends ConditionCode(7) //111


def isComment(str: String) = str.startsWith(";")
def twosComplement(int: Int, numBits: Int) = if int < 0 then int + (1 << numBits) else int
