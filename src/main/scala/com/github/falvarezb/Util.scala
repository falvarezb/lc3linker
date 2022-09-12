package com.github.falvarezb

import cats.instances.either
import cats.syntax.either.*

import scala.collection.mutable

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

  def parseBlockOfWordsSize(token: String, lineNumber: LineNumber): Either[String, Int] =
    // same validation as memory address
    parseMemoryAddress(token, lineNumber)

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

  /**
   * Detect escape sequences in the given string and replace them by the corresponding escape character, e.g.
   * "hi\\nbye" = ['h','i','\\','n','b','y','e'] is transformed into
   * "hi\nbye" = ['h','i','\n','b','y','e']
   */
  def interpretEscapeSequence(str: String, lineNumber: LineNumber): Either[String, String] =
    def loop(loopStr: List[Char], escapeSequenceMode: Boolean, newStr: List[Char]): Either[String, List[Char]] = loopStr match
      case Nil =>
        // adding null character
        ('\u0000' :: newStr).asRight[String]
      case ch :: remainingChars =>
        //https://en.wikipedia.org/wiki/Escape_sequences_in_C
        if escapeSequenceMode then
          val replacementEither = ch match
            case 'a' => '\u0007'.asRight[String]
            case 'b' => '\b'.asRight[String]
            case 'e' => '\u001b'.asRight[String]
            case 'f' => '\f'.asRight[String]
            case 'n' => '\n'.asRight[String]
            case 'r' => '\r'.asRight[String]
            case 't' => '\t'.asRight[String]
            case 'v' => '\u000b'.asRight[String]
            case '\\' => '\\'.asRight[String]
            case '"' => '"'.asRight[String]
            case _ => ().asLeft[Char]
          replacementEither match
            case Left(_) => s"ERROR (line ${lineNumber.value}): Unrecognised escape sequence ('$str')".asLeft[List[Char]]
            case Right(replacement) =>
              loop(remainingChars, false, replacement :: newStr)
              else if ch == '\\' then loop(remainingChars, true, newStr)
              else loop(remainingChars, escapeSequenceMode, ch :: newStr)

    loop(str.toList, false, Nil).map(_.reverse.mkString)


}
