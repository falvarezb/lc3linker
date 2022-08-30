package com.github.falvarezb

import java.io.FileOutputStream
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import cats.*
import cats.implicits.*
import com.github.falvarezb.Util.parseMemoryAddress

object Assembler:

  def assemble(asmFileNamePath: String): Either[String, Unit] =
    for
      linesMetadata <- doLexicalAnalysis(asmFileNamePath).asRight[String]
      tuple <- createSymbolTable(linesMetadata)
      instructions <- doSyntaxAnalysis(tuple._1, tuple._2)
      _ <- serializeInstructions(instructions, asmFileNamePath).asRight[String]
    yield ()

  def filterNotLinesAfterEnd(tokenizedLines: Iterator[LineMetadata]): List[LineMetadata] =
    def loop(allTokenizedLines: Iterator[LineMetadata], tokenizedLinesBeforeEnd: List[LineMetadata]): List[LineMetadata] =
      if allTokenizedLines.hasNext then
        val nextLine = allTokenizedLines.next()
        if nextLine.tokenizedLine(0) == ".END" then tokenizedLinesBeforeEnd
        else loop(allTokenizedLines, nextLine :: tokenizedLinesBeforeEnd)
      else tokenizedLinesBeforeEnd

    loop(tokenizedLines, Nil).reverse

  def doLexicalAnalysis(asmFileNamePath: String): List[LineMetadata] =
    val source = Source.fromFile(asmFileNamePath)
    val tokenizedLines = source.getLines().map(_.split("[ ,]").filterNot(_.isEmpty)).filterNot(_.isEmpty).zipWithIndex.map {
      case (tokenizedLine, idx) => LineMetadata(tokenizedLine, LineNumber(idx))
    }
    filterNotLinesAfterEnd(tokenizedLines)

  def createSymbolTable(linesMetadata: Seq[LineMetadata]): Either[String, (List[InstructionMetadata], Map[String, InstructionNumber])] =
    val instructionsMetadata = mutable.ListBuffer.empty[InstructionMetadata]
    val symbolTable = mutable.HashMap.empty[String, InstructionNumber]

    def loop(linesMetadata: Seq[LineMetadata], instructionNumber: InstructionNumber): Either[String, Unit] =
      linesMetadata match
        case Nil => Right(())
        case x :: xs => x match
          case lineMetadata if lineMetadata.tokenizedLine(0) == ".ORIG" =>
            parseOrig(lineMetadata.tokenizedLine, lineMetadata.lineNumber).map(InstructionNumber.apply) match
              case Left(str) => Left(str)
              case Right(initialInstructionNumber) =>
                instructionsMetadata += InstructionMetadata(lineMetadata, initialInstructionNumber)
                loop(xs, initialInstructionNumber)
          case lineMetadata if lineMetadata.isOpCode || lineMetadata.isDirective || lineMetadata.isComment =>
            instructionsMetadata += InstructionMetadata(lineMetadata, instructionNumber ∆+ 1)
            loop(xs, instructionNumber ∆+ 1)
          case lineMetadata =>
            symbolTable += (lineMetadata.tokenizedLine(0) -> instructionNumber)
            loop(xs, instructionNumber)


    loop(linesMetadata, InstructionNumber(0)).map(_ => (instructionsMetadata.toList, symbolTable.toMap))


  def doSyntaxAnalysis(instructionsMetadata: List[InstructionMetadata], symbolTable: Map[String, InstructionNumber]): Either[String,List[Int]] =
    val l: List[Either[String, Int]] = instructionsMetadata.map { instructionMetadata =>
      val firstToken = instructionMetadata.lineMetadata.tokenizedLine(0)
      firstToken match
        case ".ORIG" => Some(parseOrig(instructionMetadata.lineMetadata.tokenizedLine, instructionMetadata.lineMetadata.lineNumber))
        case "ADD" => Some(parseAdd(instructionMetadata.lineMetadata.tokenizedLine))
        case "JSR" => Some(parseJsr(instructionMetadata, symbolTable))
        case "HALT" => Some(Right(0xf025))
        case _ => None
    }.filterNot(_.isEmpty).map(_.get)
    l.sequence


  def serializeInstructions(instructions: List[Int], asmFileNamePath: String): Unit =
    val asmFileNameWithoutExtension = asmFileNamePath.split('.')(0)
    val objFile = new FileOutputStream(s"$asmFileNameWithoutExtension.obj")

    instructions.foreach { instr =>
      println(instr.toHexString)
      //JVM's big-endian representation
      //(1 << n) - 1 = 2^n - 1 = 111.. (n times) ..111
      val (mostSignificantByte, leastSignificantByte) = (instr >> 8, instr & ((1 << 8) - 1))
      objFile.write(mostSignificantByte)
      objFile.write(leastSignificantByte)
    }
    objFile.close()

  def parseOrig(tokens: Array[String], lineNumber: LineNumber): Either[String, Int] =
    if tokens.length < 2 then Left(s"ERROR (line ${lineNumber.value}): Immediate expected")
    else parseMemoryAddress(tokens(1), lineNumber)

  def parseJsr(instructionMetadata: InstructionMetadata, symbolTable: Map[String, InstructionNumber]): Either[String, Int] =
    val label = instructionMetadata.lineMetadata.tokenizedLine(1)
    val offset = (symbolTable(label) - instructionMetadata.instructionNumber) ∇- 1
    Right {
      (4 << 12) +
        (1 << 11) +
        offset
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


