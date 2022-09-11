package com.github.falvarezb

import java.io.FileOutputStream
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import cats.instances.either
import cats.syntax.either.*
import cats.syntax.all.toTraverseOps
import com.github.falvarezb.Util.parseMemoryAddress
import com.github.falvarezb.Parsers.*

import scala.annotation.tailrec

class Assembler(val symbolTable: mutable.HashMap[String, InstructionMemoryAddress]):

  def assemble(asmFileNamePath: String): Either[String, Unit] =
    for
      linesMetadata <- doLexicalAnalysis(asmFileNamePath).asRight[String]
      tuple <- createSymbolTable(linesMetadata)
      instructions <- doSyntaxAnalysis(tuple._1, tuple._2)
    yield serializeInstructions(instructions, asmFileNamePath)

  def filterNotLinesAfterEnd(lineIterator: Iterator[LineMetadata]): List[LineMetadata] =
    @tailrec
    def loop(allTokenizedLines: Iterator[LineMetadata], tokenizedLinesBeforeEnd: List[LineMetadata]): List[LineMetadata] =
      if allTokenizedLines.hasNext then
        val nextLine = allTokenizedLines.next()
        if nextLine.tokenizedLine.head == ".END" then tokenizedLinesBeforeEnd
        else loop(allTokenizedLines, nextLine :: tokenizedLinesBeforeEnd)
      else tokenizedLinesBeforeEnd

    loop(lineIterator, Nil).reverse

  def doLexicalAnalysis(asmFileNamePath: String): List[LineMetadata] =
    val source = Source.fromFile(asmFileNamePath)
    val linesWithMetadata = source.getLines()
      .map(line => (line, line.split("""[ ,"]""").filterNot(_.isEmpty).toList)) // line tokenization (empty tokens are discarded)
      .zipWithIndex // adding line number
      .filterNot { case ((_, tokenizedLine), _) => tokenizedLine.isEmpty} // removing blank lines
      .filterNot { case ((_, tokenizedLine), _) => tokenizedLine.head.startsWith(";")} // removing comments
      .map { case ((line, tokenizedLine), idx) => LineMetadata(line, tokenizedLine, LineNumber(idx+1))}

    filterNotLinesAfterEnd(linesWithMetadata)
    //result.foreach(line => println(line.tokenizedLine.mkString(" ")))
    //result

  def createSymbolTable(linesMetadata: List[LineMetadata]): Either[String, (List[InstructionMetadata], Map[String, InstructionMemoryAddress])] =
    val instructionsMetadata = mutable.ListBuffer.empty[InstructionMetadata]

    @tailrec
    def loop(lines: List[LineMetadata], instructionMemoryAddress: InstructionMemoryAddress, isLabelLine: Boolean = false): Either[String, Unit] =
      lines match
        case Nil => ().asRight[String]
        case firstLine :: remainingLines => firstLine match
          case line if line.tokenizedLine.headOption.contains(".ORIG") =>
            parseOrig(line).map(InstructionMemoryAddress.apply) match
              case Left(str) => str.asLeft[Unit]
              case Right(initialInstructionNumber) =>
                // this is not a real instruction to be executed but the memory address where LC-3 is to load the program
                // that's why initialInstructionNumber is not incremented when invoking loop
                instructionsMetadata += InstructionMetadata(line, initialInstructionNumber)
                loop(remainingLines, initialInstructionNumber)

          case line if !line.isComment && instructionMemoryAddress == InstructionMemoryAddress(0) =>
            // instruction not preceded by .ORIG directive
            s"ERROR (line 4): Instruction not preceeded by a .orig directive".asLeft[Unit]

          case line if line.tokenizedLine.headOption.contains(".STRINGZ") =>
            parseStringz(line).map(_.map(InstructionMemoryAddress.apply)) match
              case Left(str) => str.asLeft[Unit]
              case Right(instructionExpansionList) =>
                instructionExpansionList.foreach { instructionsMetadata += InstructionMetadata(line, _)}
                loop(remainingLines, instructionMemoryAddress ∆+ instructionExpansionList.length)

          case line if line.isOpCode || line.isDirective || line.isComment =>
            instructionsMetadata += InstructionMetadata(line, instructionMemoryAddress)
            loop(remainingLines, instructionMemoryAddress ∆+ 1)

          case line =>
            line.tokenizedLine.headOption match
              case Some(label) if isLabelLine =>
                // two labels in the same line is illegal
                s"ERROR (line ${line.lineNumber.value}): Invalid opcode ('$label')".asLeft[Unit]
              case Some(label) =>
                symbolTable += (label -> instructionMemoryAddress)
                // process the rest of the line after removing the label
                // rest of the line may be empty or not
                loop(line.copy(tokenizedLine = line.tokenizedLine.drop(1)) :: remainingLines, instructionMemoryAddress, true)
              case None =>
                // empty line
                loop(remainingLines, instructionMemoryAddress)

    loop(linesMetadata, InstructionMemoryAddress(0)).map(_ => (instructionsMetadata.toList, symbolTable.toMap))


  def doSyntaxAnalysis(instructionsMetadata: List[InstructionMetadata], symbolTable: Map[String, InstructionMemoryAddress]): Either[String, List[Int]] =
    val l: List[Either[String, Int]] = instructionsMetadata.map { instructionMetadata =>
      val firstToken = instructionMetadata.lineMetadata.tokenizedLine.head
      firstToken match
        case ".ORIG" => Some(parseOrig(instructionMetadata.lineMetadata))
        case "ADD" => Some(parseAdd(instructionMetadata.lineMetadata.tokenizedLine))
        case "JSR" => Some(parseJsr(instructionMetadata, symbolTable))
        case "HALT" => Some(Right(0xf025))
        case ".STRINGZ" => Some(Right(instructionMetadata.instructionMemoryAddress.value))
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




