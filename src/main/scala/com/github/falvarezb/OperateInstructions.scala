package com.github.falvarezb

import com.github.falvarezb.OpCode.{ADD, AND}
import com.github.falvarezb.Util.{parseImmediate, parseNumericValue, parseRegister, twosComplement}

object OperateInstructions:
  def parseAdd(using lineMetadata: LineMetadata): Either[String, Int] = parseAddAnd(ADD)

  def parseAnd(using lineMetadata: LineMetadata): Either[String, Int] = parseAddAnd(AND)

  def parseNot(using lineMetadata: LineMetadata): Either[String, Int] =
    val tokens = lineMetadata.tokenizedLine
    for
      _ <- Either.cond(tokens.length >= 3, (), s"ERROR (${lineMetadata.fileName} - line ${lineMetadata.lineNumber.value}): missing operands")
      DR <- parseRegister(tokens(1)).map(_ << 9)
      SR <- parseRegister(tokens(2)).map(_ << 6)
    yield (9 << 12) + DR + SR + 63

  private def parseAddAnd(opCode: OpCode)(using lineMetadata: LineMetadata): Either[String, Int] =
    assert(opCode == ADD || opCode == AND)
    val tokens = lineMetadata.tokenizedLine
    val immediateNumBits = 5

    for
      _ <- Either.cond(tokens.length >= 4, (), s"ERROR (${lineMetadata.fileName} - line ${lineMetadata.lineNumber.value}): missing operands")
      DR <- parseRegister(tokens(1)).map(_ << 9)
      SR1 <- parseRegister(tokens(2)).map(_ << 6)
      // register or immediate value?
      operand <- parseRegister(tokens(3)).orElse {
        parseImmediate(tokens(3), immediateNumBits).map { num =>
          (1 << 5) + twosComplement(num, immediateNumBits)
        }
      }
    yield (if opCode == ADD then 1 << 12 else 5 << 12) + DR + SR1 + operand

