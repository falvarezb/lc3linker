package com.github.falvarezb

import cats.*
import cats.implicits.*

object Util {

  def parseNumericValue(token: String, lineNumber: LineNumber): Either[String, Int] =
    Either.catchOnly[NumberFormatException] {
      token(0) match
        case '#' =>
          //decimal literal
          Integer.parseInt(token.substring(1))
        case 'x' =>
          //hex literal
          Integer.parseInt(token.substring(1), 16)
        case _ =>
          //decimal literal without prefix
          Integer.parseInt(token)
    }.leftMap(_ => s"ERROR (line ${lineNumber.value}): Immediate $token is not a numeric value")

  def validateNumberRange(value: Int, lineNumber: LineNumber, lowerBound: Int, upperBound: Int): Either[String, Unit] =
    if value < lowerBound || value > upperBound then
      s"ERROR (line ${lineNumber.value}): Immediate operand ($value) outside of range ($lowerBound to $upperBound)".asLeft[Unit]
    else ().asRight[String]


  def parseMemoryAddress(token: String, lineNumber: LineNumber): Either[String, Int] =
    for
      num <- parseNumericValue(token, lineNumber)
      _ <- validateNumberRange(num, lineNumber, 0, 0xFFFF)
    yield num

  def parseOffset(token: String, lineNumber: LineNumber, instructionMemoryAddress: InstructionMemoryAddress, offsetNumBits: Int, symbolTable: Map[String, InstructionMemoryAddress]): Either[String, Int] =
    def twosComplement(offset: Int) = if offset < 0 then offset + (1 << offsetNumBits) else offset
    
    for
      //is token a label or a number?
      offset <- parseNumericValue(token, lineNumber).orElse {
        ((symbolTable(token) - instructionMemoryAddress) ∇- 1).asRight[String]
      }
      _ <- validateNumberRange(offset, lineNumber, -(1 << (offsetNumBits - 1)), (1 << (offsetNumBits - 1)) - 1)
    yield twosComplement(offset)


}
