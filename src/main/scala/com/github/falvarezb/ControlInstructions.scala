package com.github.falvarezb

import com.github.falvarezb.OpCode.{JMP, JMPT, JSRR}
import com.github.falvarezb.Util.{interpretEscapeSequence, parseOffset, parseRegister, parseTrapVector}

object ControlInstructions:
  def parseJsr(instructionMetadata: InstructionMetadata, symbolTable: SymbolTable): Either[String, Int] =
    val offsetNumBits = 11
    val tokens = instructionMetadata.lineMetadata.tokenizedLine
    val lineNumber = instructionMetadata.lineMetadata.lineNumber
    val fileName = instructionMetadata.lineMetadata.fileName

    for
      _ <- Either.cond(tokens.length >= 2, (), s"ERROR ($fileName - line ${lineNumber.value}): Immediate expected")
      offset <- parseOffset(tokens(1), lineNumber, fileName, instructionMetadata.instructionLocation, offsetNumBits, symbolTable)
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
    val fileName = lineMetadata.fileName

    for
      _ <- Either.cond(tokens.length >= 2, (), s"ERROR ($fileName - line ${lineNumber.value}): Register expected")
      baseRegister <- parseRegister(tokens(1), lineNumber, fileName).map(_ << 6)
    yield (if opCode == JSRR then 4 << 12 else 12 << 12) + baseRegister + (if opCode == JMPT then 1 else 0)

  def parseBr(instructionMetadata: InstructionMetadata, symbolTable: SymbolTable, conditionCode: ConditionCode): Either[String, Int] =
    val tokens = instructionMetadata.lineMetadata.tokenizedLine
    val lineNumber = instructionMetadata.lineMetadata.lineNumber
    val fileName = instructionMetadata.lineMetadata.fileName
    val numTokens = 2
    val offsetNumBits = 9

    for
      _ <- Either.cond(tokens.length >= numTokens, (), s"ERROR ($fileName - line ${lineNumber.value}): missing operands")
      offset <- parseOffset(tokens(1), lineNumber, fileName, instructionMetadata.instructionLocation, offsetNumBits, symbolTable)
    yield (conditionCode.value << 9) + offset


  def parseTrap(lineMetadata: LineMetadata): Either[String, Int] =
    val tokens = lineMetadata.tokenizedLine
    val lineNumber = lineMetadata.lineNumber
    val fileName = lineMetadata.fileName

    for
      _ <- Either.cond(tokens.length >= 2, (), s"ERROR (line ${lineNumber.value}): missing operands")
      trapVector <- parseTrapVector(tokens(1), lineNumber, fileName)
    yield (15 << 12) + trapVector
