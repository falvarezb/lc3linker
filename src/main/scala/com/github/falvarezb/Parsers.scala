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

  def parseStringz(lineMetadata: LineMetadata): Either[String, List[Int]] =
    val tokens = lineMetadata.tokenizedLine
    val lineNumber = lineMetadata.lineNumber
    if tokens.length < 2 then
      s"ERROR (line ${lineNumber.value}): Bad string".asLeft[List[Int]]
    else
      val line = lineMetadata.line
      val firstQuotationMarkIdx = line.indexOf('"')
      val secondQuotationMarkIdx = line.lastIndexOf('"')
      val quotedContent = line.substring(firstQuotationMarkIdx+1, secondQuotationMarkIdx)
      val stringzIdx = line.indexOfSlice(".STRINGZ")
      val contentOutsideQuotationMark =
        line.substring(stringzIdx + ".STRINGZ".length, firstQuotationMarkIdx).trim.nonEmpty ||
          line.substring(secondQuotationMarkIdx + 1).headOption.exists(_ != ';')

      for
        _ <- Either.cond(!contentOutsideQuotationMark, Nil, s"ERROR (line ${lineNumber.value}): Bad string ('$line')")
        str <- interpretEscapeSequence(quotedContent, lineNumber)
      yield str.toList.map(_.toInt)




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
