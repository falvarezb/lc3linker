package com.github.falvarezb

import com.github.falvarezb.OpCode.{JMP, JMPT, JSRR}
import com.github.falvarezb.Util.{parseOffset, parseRegister}

object ControlInstructions:
  def parseJsr(instructionMetadata: InstructionMetadata, symbolTable: SymbolTable): Either[String, Int] =
    val offsetNumBits = 11
    val tokens = instructionMetadata.lineMetadata.tokenizedLine
    val lineNumber = instructionMetadata.lineMetadata.lineNumber

    for
      _ <- Either.cond(tokens.length >= 2, (), s"ERROR (line ${lineNumber.value}): Immediate expected")
      offset <- parseOffset(tokens(1), lineNumber, instructionMetadata.instructionLocation, offsetNumBits, symbolTable)
    yield (4 << 12) + (1 << 11) + offset

  def parseJsrr(lineMetadata: LineMetadata): Either[String, Int] =
    jumpInstruction(lineMetadata, JSRR)

  def parseJmp(lineMetadata: LineMetadata): Either[String, Int] =
    jumpInstruction(lineMetadata, JMP)

  def parseJmpt(lineMetadata: LineMetadata): Either[String, Int] =
    jumpInstruction(lineMetadata, JMPT)

  private def jumpInstruction(lineMetadata: LineMetadata, opCode: OpCode): Either[String, Int] =
    assert(opCode == JSRR || opCode == JMP || opCode == JMPT)
    val tokens = lineMetadata.tokenizedLine
    val lineNumber = lineMetadata.lineNumber

    for
      _ <- Either.cond(tokens.length >= 2, (), s"ERROR (line ${lineNumber.value}): Register expected")
      baseRegister <- parseRegister(tokens(1), lineNumber).map(_ << 6)
    yield (if opCode == JSRR then 4 << 12 else 12 << 12) + baseRegister + (if opCode == JMPT then 1 else 0)
  
