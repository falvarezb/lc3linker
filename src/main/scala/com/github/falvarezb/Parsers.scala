package com.github.falvarezb

import com.github.falvarezb.Util.{interpretEscapeSequence, parseBlockOfWordsSize, parseMemoryAddress, parseNumericValue, parseOffset, parseRegister, validateNumberRange}
import com.github.falvarezb.OpCode.*
import scala.collection.mutable
//import cats.*
//import cats.implicits.*
import cats.instances.either
import cats.syntax.either.*

object Parsers {

  sealed trait Cache[T]:
    val value: mutable.Map[LineMetadata, Either[String, T]] = mutable.Map.empty

  private object IntCache extends Cache[Int]
  private object IntListCache extends Cache[List[Int]]
  private def withCache[T](cache: Cache[T], lineMetadata: LineMetadata)(computation: => Either[String,T]): Either[String,T] =
    cache.value.get(lineMetadata) match
      case Some(value) => value
      case None =>
        cache.value += (lineMetadata -> computation)
        cache.value(lineMetadata)


  def parseOrig(lineMetadata: LineMetadata): Either[String, Int] = withCache(IntCache, lineMetadata) {
    val tokens = lineMetadata.tokenizedLine
    val lineNumber = lineMetadata.lineNumber
    if tokens.length < 2 then Left(s"ERROR (line ${lineNumber.value}): Immediate expected")
    else parseMemoryAddress(tokens(1), lineNumber)
  }

  /**
   * .FILL operand may be an integer [-32768, 32767] or a label of a memory address [0, 65535]
   * @param lineMetadata
   * @param symbolTable
   * @return
   */
  def parseFill(lineMetadata: LineMetadata, symbolTable: Map[String, InstructionLocation]): Either[String, Int] =
    val tokens = lineMetadata.tokenizedLine
    val lineNumber = lineMetadata.lineNumber
    for
      _ <- Either.cond(tokens.length >= 2, (), s"ERROR (line ${lineNumber.value}): Immediate expected")
      operand = tokens(1)
      //is token a label or a number?
      num <- parseNumericValue(operand, lineNumber).orElse {
        Either.catchOnly[NoSuchElementException] {symbolTable(operand)}
          .map(_.value)
          .leftMap(_ => s"ERROR (line ${lineNumber.value}): Symbol not found ('$operand')")
      }
      _ <- validateNumberRange(operand, num, lineNumber, -32768, 65535)
    yield num

  def stringzAllocatedMemory(lineMetadata: LineMetadata): Either[String, Int] =
    parseStringz(lineMetadata).map(_.length)

  /**
   * Parse .STRINGZ directive to store the ASCII representation of each of the chars of the string
   *
   * When reading the content of the asm file and storing it in a string, special characters are escaped
   * according to https://en.wikipedia.org/wiki/Escape_sequences_in_C.
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
  def parseStringz(lineMetadata: LineMetadata): Either[String, List[Int]] = withCache(IntListCache, lineMetadata) {
    def isAsciiChar(ch: Char) = ch < 128

    val lineNumber = lineMetadata.lineNumber
    val line = lineMetadata.line
    val firstQuotationMarkIdx = line.indexOf('"')
    val secondQuotationMarkIdx = line.lastIndexOf('"')

    for
      _ <- Either.cond(!(firstQuotationMarkIdx == -1 || secondQuotationMarkIdx == firstQuotationMarkIdx), (), s"ERROR (line ${lineNumber.value}): Bad string ('$line')")
      quotedContent = line.substring(firstQuotationMarkIdx + 1, secondQuotationMarkIdx)
      stringzIdx = line.indexOfSlice(".STRINGZ")
      contentOutsideQuotationMark =
        line.substring(stringzIdx + ".STRINGZ".length, firstQuotationMarkIdx).trim.nonEmpty ||
          line.substring(secondQuotationMarkIdx + 1).headOption.exists(_ != ';')
      _ <- Either.cond(!contentOutsideQuotationMark, Nil, s"ERROR (line ${lineNumber.value}): Bad string ('$line')")
      str <- interpretEscapeSequence(quotedContent, lineNumber)
      _ <- Either.cond(str.forall(isAsciiChar), Nil, s"ERROR (line ${lineNumber.value}): Bad string, non-ascii char ('$line')")
    yield str.toList.map(_.toInt)
  }

  def blkwAllocatedMemory(lineMetadata: LineMetadata): Either[String, Int] =
    parseBlkw(lineMetadata).map(_.length)

  def parseBlkw(lineMetadata: LineMetadata): Either[String, List[Int]] = withCache(IntListCache, lineMetadata) {
    val tokens = lineMetadata.tokenizedLine
    val lineNumber = lineMetadata.lineNumber

    for
      _ <- Either.cond(tokens.length >= 2, (), s"ERROR (line ${lineNumber.value}): Immediate expected")
      block_size <- parseBlockOfWordsSize(tokens(1), lineMetadata.lineNumber)
      instructions <- List.fill(block_size)(0).asRight[String]
    yield instructions
  }


  def parseJsr(instructionMetadata: InstructionMetadata, symbolTable: Map[String, InstructionLocation]): Either[String, Int] =
    val offsetNumBits = 11
    val tokens = instructionMetadata.lineMetadata.tokenizedLine
    val lineNumber = instructionMetadata.lineMetadata.lineNumber

    for
      _ <- Either.cond(tokens.length >= 2, (), s"ERROR (line ${lineNumber.value}): Immediate expected")
      offset <- parseOffset(tokens(1), lineNumber, instructionMetadata.instructionLocation, offsetNumBits, symbolTable)
    yield (4 << 12) + (1 << 11) + offset

  def parseJsrr(lineMetadata: LineMetadata): Either[String, Int] =
    jumpInstruction(lineMetadata, JSRR)

  def parseJmp(lineMetadata: LineMetadata): Either[String, Int] =
    jumpInstruction(lineMetadata, JMP)

  def parseJmpt(lineMetadata: LineMetadata): Either[String, Int] =
    jumpInstruction(lineMetadata, JMPT)
    
  private def jumpInstruction(lineMetadata: LineMetadata, opCode: OpCode): Either[String, Int] =
    assert(opCode == JSRR || opCode == JMP || opCode == JMPT)
    val tokens = lineMetadata.tokenizedLine
    val lineNumber = lineMetadata.lineNumber

    for
      _ <- Either.cond(tokens.length >= 2, (), s"ERROR (line ${lineNumber.value}): Register expected")
      baseRegister <- parseRegister(tokens(1), lineNumber).map(_ << 6)
    yield (if opCode == JSRR then 4 << 12 else 12 << 12)  + baseRegister + (if opCode == JMPT then 1 else 0)


  def parseAdd(lineMetadata: LineMetadata): Either[String, Int] = parseAddAnd(lineMetadata, ADD)
  def parseAnd(lineMetadata: LineMetadata): Either[String, Int] = parseAddAnd(lineMetadata, AND)

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
        for
          num <- parseNumericValue(tokens(3), lineNumber)
          _ <- validateNumberRange(tokens(3), num, lineNumber, -(1 << (immediateNumBits - 1)), (1 << (immediateNumBits - 1)) - 1)
        yield {
          (1 << 5) + twosComplement(num, immediateNumBits)
        }
      }
    yield (if opCode == ADD then 1 << 12 else 5 << 12) + DR + SR1 + operand

  def parseNot(lineMetadata: LineMetadata): Either[String, Int] =
    val tokens = lineMetadata.tokenizedLine
    val lineNumber = lineMetadata.lineNumber

    for
      _ <- Either.cond(tokens.length >= 3, (), s"ERROR (line ${lineNumber.value}): missing operands")
      DR <- parseRegister(tokens(1), lineNumber).map(_ << 9)
      SR <- parseRegister(tokens(2), lineNumber).map(_ << 6)
    yield (9 << 12) + DR + SR + 63

  def parseLdr(instructionMetadata: InstructionMetadata, symbolTable: Map[String, InstructionLocation]) =
    baseRegisterPlusOffsetAddressingMode(instructionMetadata, symbolTable, LDR)

  def parseStr(instructionMetadata: InstructionMetadata, symbolTable: Map[String, InstructionLocation]) =
    baseRegisterPlusOffsetAddressingMode(instructionMetadata, symbolTable, STR)
  private def baseRegisterPlusOffsetAddressingMode(instructionMetadata: InstructionMetadata, symbolTable: Map[String, InstructionLocation], opCode: OpCode) =
    assert(opCode == LDR || opCode == STR)
    val tokens = instructionMetadata.lineMetadata.tokenizedLine
    val lineNumber = instructionMetadata.lineMetadata.lineNumber
    val offsetNumBits = 6

    for
      _ <- Either.cond(tokens.length >= 4, (), s"ERROR (line ${lineNumber.value}): missing operands")
      SR_DR <- parseRegister(tokens(1), lineNumber).map(_ << 9)
      baseRegister <- parseRegister(tokens(2), lineNumber).map(_ << 6)
      offset <- parseOffset(tokens(3), lineNumber, instructionMetadata.instructionLocation, offsetNumBits, symbolTable)

    yield (if opCode == LDR then 6 << 12 else 7 << 12) + SR_DR + baseRegister + offset
}
