package com.github.falvarezb

import cats.instances.either
import cats.syntax.either.*

import scala.collection.mutable

object Util:

  /**
   * Integer literals in LC-3 assembly language can be represented as:
   *
   * - decimal values, either prefixed by '#' or without prefix
   *
   * - hex values prefixed by 'x'
   */
  private def parseNumericValue(token: String)(using lineMetadata: LineMetadata): Either[String, Int] =
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
    }.leftMap(_ => s"ERROR (${lineMetadata.fileName} - line ${lineMetadata.lineNumber.value}): Immediate $token is not a numeric value")

  private def validateNumberRange(token: String, value: Int, lowerBound: Int, upperBound: Int)(using lineMetadata: LineMetadata): Either[String, Unit] =
    Either.cond(value >= lowerBound && value <= upperBound, (), s"ERROR (${lineMetadata.fileName} - line ${lineMetadata.lineNumber.value}): Immediate operand ($token) out of range ($lowerBound to $upperBound)")

  /**
   * Parse token to obtain a memory address, that is, a numeric value in the range [0, 0xFFFF]
   */
  def parseMemoryAddress(token: String)(using lineMetadata: LineMetadata): Either[String, Int] =
    parseNumericValue(token, 0, 0xFFFF)

  /**
   * Parse token corresponding to the size specified by a .BLKW directive
   */
  def parseBlockOfWordsSize(token: String)(using lineMetadata: LineMetadata): Either[String, Int] =
  // same validation as memory address
    parseMemoryAddress(token)

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
  def parseOffset(token: String, instructionMemoryAddress: InstructionLocation, offsetNumBits: Int, symbolTable: SymbolTable)(using lineMetadata: LineMetadata): Either[String, Int] =
    parseNumericValueWithAlternativeParser(token, -(1 << (offsetNumBits - 1)), (1 << (offsetNumBits - 1)) - 1) {
      Some(
        Either.catchOnly[NoSuchElementException] {
          symbolTable(token)
        }
          .map(symbolicNameValue => (symbolicNameValue - instructionMemoryAddress) ∇- 1)
          .leftMap(_ => s"ERROR (${lineMetadata.fileName} - line ${lineMetadata.lineNumber.value}): Symbol not found ('$token')")
      )
    }.map(offset => twosComplement(offset, offsetNumBits))

  /**
   * Detect escape sequences in the given string and replace them by the corresponding escape character, e.g.
   * "hi\\nbye" = ['h','i','\\','n','b','y','e'] is transformed into
   * "hi\nbye" = ['h','i','\n','b','y','e']
   */
  def interpretEscapeSequence(str: String)(using lineMetadata: LineMetadata): Either[String, String] =
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
            case Left(_) => s"ERROR (${lineMetadata.fileName} - line ${lineMetadata.lineNumber.value}): Unrecognised escape sequence ('$str')".asLeft[List[Char]]
            case Right(replacement) =>
              loop(remainingChars, false, replacement :: newStr)
              else if ch == '\\' then loop(remainingChars, true, newStr)
            else loop(remainingChars, escapeSequenceMode, ch :: newStr)

    loop(str.toList, false, Nil).map(_.reverse.mkString)

  /**
   * Parse token as a register, returning the corresponding numeric value:
   *
   * R0, R1, R2, R3, R4, R5, R6, R7 correspond to 0, 1, 2, 3, 4, 5, 6, 7 respectively
   */
  def parseRegister(token: String)(using lineMetadata: LineMetadata): Either[String, Int] =
    Either.cond(token.length == 2 && token.head == 'R' && token(1) >= '0' && token(1) <= '7', token(1).toString.toInt, s"ERROR (${lineMetadata.fileName} - line ${lineMetadata.lineNumber.value}): Expected register but found $token")

  /**
   * Parse token as a numeric value, validating that it is in the range [lowerBound,upperBound]
   *
   * If default parser fails, the alternative parser 'altParser' is applied
   */
  def parseNumericValueWithAlternativeParser(token: String, lowerBound: Int, upperBound: Int)(using lineMetadata: LineMetadata)(altParser: => Option[Either[String, Int]]): Either[String, Int] =
    val parsedValue: Either[String, Int] = parseNumericValue(token)
    for
      num <- altParser match
        case Some(altp) => parsedValue.orElse(altp)
        case None => parsedValue
      _ <- validateNumberRange(token, num, lowerBound, upperBound)
    yield num

  /**
   * Parse token as a numeric value, validating that it is in the range [lowerBound,upperBound]
   */
  private def parseNumericValue(token: String, lowerBound: Int, upperBound: Int)(using lineMetadata: LineMetadata): Either[String, Int] =
    parseNumericValueWithAlternativeParser(token, lowerBound, upperBound)(None)

  def parseImmediate(token: String, immediateNumBits: Int)(using lineMetadata: LineMetadata): Either[String, Int] =
    parseNumericValue(token, -(1 << (immediateNumBits - 1)), (1 << (immediateNumBits - 1)) - 1)

  def parseTrapVector(token: String)(using lineMetadata: LineMetadata): Either[String, Int] =
    parseNumericValue(token, 0, 0xFF)

