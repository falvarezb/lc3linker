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
import scala.util.Using

class Assembler(val symbolTable: mutable.HashMap[String, InstructionLocation]):

  def assemble(asmFileNamePath: String): Either[String, Unit] =
    for
      linesMetadata <- doLexicalAnalysis(asmFileNamePath)
      tuple <- createSymbolTable(linesMetadata)
      instructions <- doSyntaxAnalysis(tuple._1, tuple._2)
    yield serializeInstructions(instructions, asmFileNamePath)

  def doLexicalAnalysis(asmFileNamePath: String) =
    Using(Source.fromFile(asmFileNamePath)) { source =>
      source.getLines()
        .map(line => (line, line.split("""[ ,"]""").filterNot(_.isEmpty).toList)) // line tokenization (empty tokens are discarded)
        .zipWithIndex // adding line number
        .filterNot { case ((_, tokenizedLine), _) => tokenizedLine.isEmpty } // removing blank lines
        .filterNot { case ((_, tokenizedLine), _) => tokenizedLine.head.startsWith(";") } // removing comments
        .takeWhile { case ((_, tokenizedLine), _) => tokenizedLine.head != ".END"} // discarding lines after '.END' directive
        .map { case ((line, tokenizedLine), idx) => LineMetadata(line, tokenizedLine, LineNumber(idx + 1)) }
        .toList
    }.toEither.leftMap(t => s"Error while reading file ${t.getMessage}")

  def createSymbolTable(linesMetadata: List[LineMetadata]): Either[String, (List[InstructionMetadata], Map[String, InstructionLocation])] =
    val instructionsMetadata = mutable.ListBuffer.empty[InstructionMetadata]

    @tailrec
    def loop(lines: List[LineMetadata], instructionLocation: InstructionLocation, isLabelLine: Boolean = false): Either[String, Unit] =
      lines match
        case Nil => ().asRight[String]
        case firstLine :: remainingLines => firstLine match
          case line if line.tokenizedLine.headOption.contains(".ORIG") =>
            parseOrig(line).map(InstructionLocation.apply) match
              case Left(str) => str.asLeft[Unit]
              case Right(initialInstructionLocation) =>
                // this is not a real instruction to be executed but the memory address where LC-3 is to load the program
                // that's why initialInstructionNumber is not incremented when invoking loop
                instructionsMetadata += InstructionMetadata(line, initialInstructionLocation)
                loop(remainingLines, initialInstructionLocation)

          case line if !line.isComment && instructionLocation == InstructionLocation(0) =>
            // instruction not preceded by .ORIG directive
            s"ERROR (line 4): Instruction not preceeded by a .orig directive".asLeft[Unit]

          case line if line.tokenizedLine.headOption.contains(".STRINGZ") =>
            parseStringz2(line) match
              case Left(str) => str.asLeft[Unit]
              case Right(allocatedMemorySize) =>
                instructionsMetadata += InstructionMetadata(line, instructionLocation)
                loop(remainingLines, instructionLocation ∆+ allocatedMemorySize)

          case line if line.tokenizedLine.headOption.contains(".BLKW") =>
            parseBlkw2(line) match
              case Left(str) => str.asLeft[Unit]
              case Right(allocatedMemorySize) =>
                instructionsMetadata += InstructionMetadata(line, instructionLocation)
                loop(remainingLines, instructionLocation ∆+ allocatedMemorySize)

          case line if line.isOpCode || line.isDirective || line.isComment =>
            instructionsMetadata += InstructionMetadata(line, instructionLocation)
            loop(remainingLines, instructionLocation ∆+ 1)

          case line => //label
            line.tokenizedLine.headOption match
              case Some(label) if isLabelLine =>
                // two labels in the same line is illegal
                s"ERROR (line ${line.lineNumber.value}): Invalid opcode ('$label')".asLeft[Unit]
              case Some(label) =>
                symbolTable += (label -> instructionLocation)
                // process the rest of the line after removing the label
                // rest of the line may be empty or not
                loop(line.copy(tokenizedLine = line.tokenizedLine.drop(1)) :: remainingLines, instructionLocation, true)
              case None =>
                // empty line
                loop(remainingLines, instructionLocation)

    loop(linesMetadata, InstructionLocation(0)).map(_ => (instructionsMetadata.toList, symbolTable.toMap))


  def doSyntaxAnalysis(instructionsMetadata: List[InstructionMetadata], symbolTable: Map[String, InstructionLocation]): Either[String, List[Int]] =
    val initialInstructionLocation = instructionsMetadata.head.instructionLocation
    val l: List[Either[String, List[Int]]] = instructionsMetadata.map { instructionMetadata =>
      val firstToken = instructionMetadata.lineMetadata.tokenizedLine.head
      firstToken match
        case ".ORIG" => List(initialInstructionLocation.value).asRight[String]
        case "ADD" => parseAdd(instructionMetadata.lineMetadata.tokenizedLine).map(List(_))
        case "JSR" => parseJsr(instructionMetadata, symbolTable).map(List(_))
        case "HALT" => List(0xf025).asRight[String]
        case ".STRINGZ" => parseStringz(instructionMetadata.lineMetadata)
        case ".BLKW" => parseBlkw(instructionMetadata.lineMetadata)
        case ".FILL" => parseFill(instructionMetadata.lineMetadata, symbolTable).map(List(_))
        case _ => Nil.asRight[String]
    }
    l.sequence.map(_.flatten)


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




