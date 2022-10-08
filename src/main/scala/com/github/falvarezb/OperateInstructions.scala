package com.github.falvarezb

import com.github.falvarezb.OpCode.{ADD, AND}
import com.github.falvarezb.Util.{parseImmediate, parseNumericValue, parseRegister}

object OperateInstructions:
  def parseAdd(lineMetadata: LineMetadata): Either[String, Int] = parseAddAnd(lineMetadata, ADD)

  def parseAnd(lineMetadata: LineMetadata): Either[String, Int] = parseAddAnd(lineMetadata, AND)

  def parseNot(lineMetadata: LineMetadata): Either[String, Int] =
    val tokens = lineMetadata.tokenizedLine
    val lineNumber = lineMetadata.lineNumber

    for
      _ <- Either.cond(tokens.length >= 3, (), s"ERROR (line ${lineNumber.value}): missing operands")
      DR <- parseRegister(tokens(1), lineNumber).map(_ << 9)
      SR <- parseRegister(tokens(2), lineNumber).map(_ << 6)
    yield (9 << 12) + DR + SR + 63

  private def parseAddAnd(lineMetadata: LineMetadata, opCode: OpCode): Either[String, Int] =
    assert(opCode == ADD || opCode == AND)
    val tokens = lineMetadata.tokenizedLine
    val lineNumber = lineMetadata.lineNumber
    val immediateNumBits = 5

    for
      _ <- Either.cond(tokens.length >= 4, (), s"ERROR (line ${lineNumber.value}): missing operands")
      DR <- parseRegister(tokens(1), lineNumber).map(_ << 9)
      SR1 <- parseRegister(tokens(2), lineNumber).map(_ << 6)
      // register or immediate value?
      operand <- parseRegister(tokens(3), lineNumber).orElse {
        parseImmediate(tokens(3), lineNumber, immediateNumBits).map { num =>
          (1 << 5) + twosComplement(num, immediateNumBits)
        }
      }
    yield (if opCode == ADD then 1 << 12 else 5 << 12) + DR + SR1 + operand

