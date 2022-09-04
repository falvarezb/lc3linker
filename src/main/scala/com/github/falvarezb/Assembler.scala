package com.github.falvarezb

import java.io.FileOutputStream
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import cats.*
import cats.implicits.*
import com.github.falvarezb.Util.parseMemoryAddress
import com.github.falvarezb.Parsers.*

class Assembler(val symbolTable: mutable.HashMap[String, InstructionMemoryAddress]):

  def assemble(asmFileNamePath: String): Either[String, Unit] =
    for
      linesMetadata <- doLexicalAnalysis(asmFileNamePath).asRight[String]
      tuple <- createSymbolTable(linesMetadata)
      instructions <- doSyntaxAnalysis(tuple._1, tuple._2)
    yield serializeInstructions(instructions, asmFileNamePath)

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
    val tokenizedLines = source.getLines()
      .map(_.split("[ ,]").filterNot(_.isEmpty)) // line tokenization (empty tokens are discarded)
      .zipWithIndex // adding line number
      .filterNot { case (tokenizedLine, _) => tokenizedLine.isEmpty} // removing blank lines
      .filterNot { case (tokenizedLine, _) => tokenizedLine.head.startsWith(";")} // removing comments
      .map { case (tokenizedLine, idx) => LineMetadata(tokenizedLine, LineNumber(idx+1))}
    filterNotLinesAfterEnd(tokenizedLines)
    //result.foreach(line => println(line.tokenizedLine.mkString(" ")))
    //result

  def createSymbolTable(linesMetadata: Seq[LineMetadata]): Either[String, (List[InstructionMetadata], Map[String, InstructionMemoryAddress])] =
    val instructionsMetadata = mutable.ListBuffer.empty[InstructionMetadata]
    def loop(linesMetadata: Seq[LineMetadata], instructionMemoryAddress: InstructionMemoryAddress): Either[String, Unit] =
      linesMetadata match
        case Nil => ().asRight[String]
        case x :: xs => x match
          case lineMetadata if lineMetadata.tokenizedLine(0) == ".ORIG" =>
            parseOrig(lineMetadata).map(InstructionMemoryAddress.apply) match
              case Left(str) => str.asLeft[Unit]
              case Right(initialInstructionNumber) =>
                // this is not a real instruction to be executed but the memory address where LC-3 is to load the program 
                // that's why initialInstructionNumber is not incremented when invoking loop
                instructionsMetadata += InstructionMetadata(lineMetadata, initialInstructionNumber)
                loop(xs, initialInstructionNumber)
          case lineMetadata if lineMetadata.isOpCode || lineMetadata.isDirective =>
            instructionsMetadata += InstructionMetadata(lineMetadata, instructionMemoryAddress)
            loop(xs, instructionMemoryAddress ∆+ 1)
          case lineMetadata =>
            symbolTable += (lineMetadata.tokenizedLine(0) -> instructionMemoryAddress)
            loop(xs, instructionMemoryAddress)


    loop(linesMetadata, InstructionMemoryAddress(0)).map(_ => (instructionsMetadata.toList, symbolTable.toMap))


  def doSyntaxAnalysis(instructionsMetadata: List[InstructionMetadata], symbolTable: Map[String, InstructionMemoryAddress]): Either[String,List[Int]] =
    val l: List[Either[String, Int]] = instructionsMetadata.map { instructionMetadata =>
      val firstToken = instructionMetadata.lineMetadata.tokenizedLine(0)
      firstToken match
        case ".ORIG" => Some(parseOrig(instructionMetadata.lineMetadata))
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




