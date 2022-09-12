package com.github.falvarezb

import com.github.falvarezb.Util.{interpretEscapeSequence, parseMemoryAddress, parseNumericValue, parseOffset, validateNumberRange}

import scala.collection.mutable
//import cats.*
//import cats.implicits.*
import cats.instances.either
import cats.syntax.either.*

object Parsers {

  def parseOrig(lineMetadata: LineMetadata): Either[String, Int] =
    val tokens = lineMetadata.tokenizedLine
    val lineNumber = lineMetadata.lineNumber
    if tokens.length < 2 then Left(s"ERROR (line ${lineNumber.value}): Immediate expected")
    else parseMemoryAddress(tokens(1), lineNumber)

  /**
   * Parse .STRINGZ directive to generate the corresponding instructions: each of the chars of the string
   * results in an "instruction" whose value is the int value of the char according to the ASCII standard
   *
   * When reading the content of the asm file and storing it in a string, special characters are escaped
   * according to https://en.wikipedia.org/wiki/Escape_sequences_in_C.
   *
   * As a result, escape characters in the operand of the .STRINGZ directive lose their meaning and need
   * to be re-interpreted, e.g.
   *
   * .STRINGZ "hi\nbye" in the asm file --> ".STRINGZ \"hi\\nbye" in the program
   *
   * So effectively, the escape sequence '\n' has become 2 different characters: '\' and 'n'
   *
   * @param lineMetadata
   * @return
   */
  def parseStringz(lineMetadata: LineMetadata): Either[String, List[Int]] =
    def isAsciiChar(ch: Char) = ch < 128

    val lineNumber = lineMetadata.lineNumber
    val line = lineMetadata.line
    val firstQuotationMarkIdx = line.indexOf('"')
    val secondQuotationMarkIdx = line.lastIndexOf('"')
    if firstQuotationMarkIdx == -1 || secondQuotationMarkIdx == firstQuotationMarkIdx then
      s"ERROR (line ${lineNumber.value}): Bad string ('$line')".asLeft[List[Int]]
    else
      val quotedContent = line.substring(firstQuotationMarkIdx+1, secondQuotationMarkIdx)
      val stringzIdx = line.indexOfSlice(".STRINGZ")
      val contentOutsideQuotationMark =
        line.substring(stringzIdx + ".STRINGZ".length, firstQuotationMarkIdx).trim.nonEmpty ||
          line.substring(secondQuotationMarkIdx + 1).headOption.exists(_ != ';')

      for
        _ <- Either.cond(!contentOutsideQuotationMark, Nil, s"ERROR (line ${lineNumber.value}): Bad string ('$line')")
        str <- interpretEscapeSequence(quotedContent, lineNumber)
        _ <- Either.cond(str.forall(isAsciiChar), Nil, s"ERROR (line ${lineNumber.value}): Bad string, non-ascii char ('$line')")
      yield str.toList.map(_.toInt)

  def parseBlkw(lineMetadata: LineMetadata): Either[String, List[Int]] =
    val tokens = lineMetadata.tokenizedLine
    val lineNumber = lineMetadata.lineNumber
    if tokens.length < 2 then Left(s"ERROR (line ${lineNumber.value}): Immediate expected")
    else
      for
        block_size <- parseMemoryAddress(tokens(1), lineMetadata.lineNumber)
        instructions <- List.fill(block_size)(0).asRight[String]
      yield instructions




  def parseJsr(instructionMetadata: InstructionMetadata, symbolTable: Map[String, InstructionMemoryAddress]): Either[String, Int] =
    val offsetNumBits = 11
    val tokens = instructionMetadata.lineMetadata.tokenizedLine
    val lineNumber = instructionMetadata.lineMetadata.lineNumber
    if tokens.length < 2 then Left(s"ERROR (line ${lineNumber.value}): Immediate expected")
    else
      parseOffset(tokens(1), lineNumber, instructionMetadata.instructionMemoryAddress, offsetNumBits, symbolTable).map { offset =>
        (4 << 12) + (1 << 11) + offset
      }



  def parseAdd(tokens: List[String]): Either[String, Int] =
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
