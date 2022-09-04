package com.github.falvarezb

import com.github.falvarezb.Util.{parseMemoryAddress, parseNumericValue, parseOffset, validateNumberRange}
import cats.*
import cats.implicits.*

object Parsers {

  def parseOrig(lineMetadata: LineMetadata): Either[String, Int] =
    val tokens = lineMetadata.tokenizedLine
    val lineNumber = lineMetadata.lineNumber
    if tokens.length < 2 then Left(s"ERROR (line ${lineNumber.value}): Immediate expected")
    else parseMemoryAddress(tokens(1), lineNumber)

  def parseJsr(instructionMetadata: InstructionMetadata, symbolTable: Map[String, InstructionMemoryAddress]): Either[String, Int] =
    val offsetNumBits = 11
    val tokens = instructionMetadata.lineMetadata.tokenizedLine
    val lineNumber = instructionMetadata.lineMetadata.lineNumber
    if tokens.length < 2 then Left(s"ERROR (line ${lineNumber.value}): Immediate expected")
    else
      parseOffset(tokens(1), lineNumber, instructionMetadata.instructionMemoryAddress, offsetNumBits, symbolTable).map { offset =>
        (4 << 12) + (1 << 11) + offset
      }



  def parseAdd(tokens: Array[String]): Either[String, Int] =
    val immediateBit = if tokens(3)(0) == 'R' then 0 else 1 << 5
    //ops code: 0001
    Right {
      (1 << 12) +
        (tokens(1).substring(1).toInt << 9) +
        (tokens(2).substring(1).toInt << 6) +
        immediateBit +
        tokens(3).substring(1).toInt
    }

}
