package com.github.falvarezb

case class LineNumber(value: Int) extends AnyVal
case class InstructionNumber(value: Int) extends AnyVal:
  def +(other: Int) = InstructionNumber(value + other)
case class LineMetadata(tokenizedLine: Array[String], lineNumber: LineNumber)
case class InstructionMetadata(lineMetadata: LineMetadata, instructionNumber: InstructionNumber)