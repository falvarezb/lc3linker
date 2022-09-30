package com.github.falvarezb

import cats.instances.either
import cats.syntax.either.*

import scala.collection.mutable

object Util:

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

  def validateNumberRange(token: String, value: Int, lineNumber: LineNumber, lowerBound: Int, upperBound: Int): Either[String, Unit] =
    Either.cond(value >= lowerBound && value <= upperBound, (), s"ERROR (line ${lineNumber.value}): Immediate operand ($token) out of range ($lowerBound to $upperBound)")

  /**
   * Transforms the given token in a valid memory address, namely: an integer in the range [0, 0xFFFF]
   */
  def parseMemoryAddress(token: String, lineNumber: LineNumber): Either[String, Int] =
    parseInteger(token, lineNumber, 0, 0xFFFF)

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
  def parseOffset(token: String, lineNumber: LineNumber, instructionMemoryAddress: InstructionLocation, offsetNumBits: Int, symbolTable: SymbolTable): Either[String, Int] =
    withAlternativeParser(token, lineNumber, -(1 << (offsetNumBits - 1)), (1 << (offsetNumBits - 1)) - 1) {
      Either.catchOnly[NoSuchElementException] {
        symbolTable(token)
      }
        .map(symbolicNameValue => (symbolicNameValue - instructionMemoryAddress) ∇- 1)
        .leftMap(_ => s"ERROR (line ${lineNumber.value}): Symbol not found ('$token')")
    }.map(offset => twosComplement(offset, offsetNumBits))

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


  def parseRegister(token: String, lineNumber: LineNumber): Either[String, Int] =
    Either.cond(token.length == 2 && token.head == 'R' && token(1) >= '0' && token(1) <= '7', token(1).toString.toInt, s"ERROR (line ${lineNumber.value}): Expected register but found $token")

  def withAlternativeParser(token: String, lineNumber: LineNumber, lowerBound: Int, upperBound: Int)(altParser: => Either[String, Int]) =
    val parsedValue = parseNumericValue(token, lineNumber)
    for
      num <- parsedValue.orElse(altParser)
      _ <- validateNumberRange(token, num, lineNumber, lowerBound, upperBound)
    yield num

  def parseInteger(token: String, lineNumber: LineNumber, lowerBound: Int, upperBound: Int) =
    for
      num <- parseNumericValue(token, lineNumber)
      _ <- validateNumberRange(token, num, lineNumber, lowerBound, upperBound)
    yield num

