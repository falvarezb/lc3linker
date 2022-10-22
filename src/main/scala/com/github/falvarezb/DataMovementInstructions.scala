package com.github.falvarezb

import com.github.falvarezb.OpCode.{LD, LDI, LDR, LEA, ST, STI, STR}
import com.github.falvarezb.Util.{parseOffset, parseRegister}

object DataMovementInstructions:
  def parseLdr(instructionMetadata: InstructionMetadata, symbolTable: SymbolTable): Either[String, Int] =
    baseRegisterPlusOffsetAddressingMode(instructionMetadata, symbolTable, LDR)

  def parseStr(instructionMetadata: InstructionMetadata, symbolTable: SymbolTable): Either[String, Int] =
    baseRegisterPlusOffsetAddressingMode(instructionMetadata, symbolTable, STR)

  def parseLd(instructionMetadata: InstructionMetadata, symbolTable: SymbolTable): Either[String, Int] =
    pcRelativeAddressingMode(instructionMetadata, symbolTable, LD)

  def parseSt(instructionMetadata: InstructionMetadata, symbolTable: SymbolTable): Either[String, Int] =
    pcRelativeAddressingMode(instructionMetadata, symbolTable, ST)

  def parseLdi(instructionMetadata: InstructionMetadata, symbolTable: SymbolTable): Either[String, Int] =
    pcRelativeAddressingMode(instructionMetadata, symbolTable, LDI)

  def parseLea(instructionMetadata: InstructionMetadata, symbolTable: SymbolTable): Either[String, Int] =
    pcRelativeAddressingMode(instructionMetadata, symbolTable, LEA)

  def parseSti(instructionMetadata: InstructionMetadata, symbolTable: SymbolTable): Either[String, Int] =
    pcRelativeAddressingMode(instructionMetadata, symbolTable, STI)

  private def baseRegisterPlusOffsetAddressingMode(instructionMetadata: InstructionMetadata, symbolTable: SymbolTable, opCode: OpCode) =
    assert(opCode == LDR || opCode == STR)
    val tokens = instructionMetadata.lineMetadata.tokenizedLine
    val lineNumber = instructionMetadata.lineMetadata.lineNumber
    val fileName = instructionMetadata.lineMetadata.fileName
    val numTokens = 4
    val offsetNumBits = 6

    for
      _ <- Either.cond(tokens.length >= numTokens, (), s"ERROR ($fileName - line ${lineNumber.value}): missing operands")
      SR_DR <- parseRegister(tokens(1), lineNumber, fileName).map(_ << 9)
      baseRegister <- parseRegister(tokens(2), lineNumber, fileName).map(_ << 6)
      offset <- parseOffset(tokens(3), lineNumber, instructionMetadata.instructionLocation, offsetNumBits, symbolTable)
    yield (if opCode == LDR then 6 << 12 else 7 << 12) + SR_DR + baseRegister + offset


  private def pcRelativeAddressingMode(instructionMetadata: InstructionMetadata, symbolTable: SymbolTable, opCode: OpCode) =
    assert(opCode == LD || opCode == ST || opCode == LDI || opCode == STI || opCode == ST || opCode == LEA)
    val tokens = instructionMetadata.lineMetadata.tokenizedLine
    val lineNumber = instructionMetadata.lineMetadata.lineNumber
    val fileName = instructionMetadata.lineMetadata.fileName
    val numTokens = 3
    val offsetNumBits = 9
    val opCodeBinary = (opCode: @unchecked) match
      case LD => 2 << 12
      case ST => 3 << 12
      case LDI => 10 << 12
      case STI => 11 << 12
      case LEA => 14 << 12

    for
      _ <- Either.cond(tokens.length >= numTokens, (), s"ERROR ($fileName - line ${lineNumber.value}): missing operands")
      SR_DR <- parseRegister(tokens(1), lineNumber, fileName).map(_ << 9)
      offset <- parseOffset(tokens(2), lineNumber, instructionMetadata.instructionLocation, offsetNumBits, symbolTable)
    yield opCodeBinary + SR_DR + offset
