package com.github.falvarezb

import cats.*
import cats.implicits.*

object Util {

  /**
   * Integer literals in LC-3 assembly language can be represented as:
   * - decimal values, either prefixed by '#' or without prefix
   * - hex values prefixed by 'x'
   */
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

  /**
   * Transforms the given token in a valid memory address, namely: an integer in the range [0, 0xFFFF]
   */
  def parseMemoryAddress(token: String, lineNumber: LineNumber): Either[String, Int] =
    for
      num <- parseNumericValue(token, lineNumber)
      _ <- validateNumberRange(num, lineNumber, 0, 0xFFFF)
    yield num

  /**
   * Transforms the given token in a valid offset.
   * 
   * The token can be a symbolic name (label) or a numeric value. If a label, the corresponding memory address is
   * retrieved from the symbol table.
   * 
   * How to work out the offset:
   * - calculate difference between numeric value of the token and the instruction's memory address and then subtract 1
   * - validate the result of the previous step is within the range indicated by offsetNumBits
   * - calculate 2's complement
   */
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
