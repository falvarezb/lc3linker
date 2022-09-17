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

  def doLexicalAnalysis(asmFileNamePath: String): Either[String, List[LineMetadata]] =
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
    def processLine(line: LineMetadata, instructionLocation: InstructionLocation, isLabelLine: Boolean = false): Either[String, Int] =
      line match
        case _ if line.tokenizedLine.head.contains(".ORIG") => parseOrig(line)
        case _ if line.tokenizedLine.head.contains(".STRINGZ") => parseStringz2(line)
        case _ if line.tokenizedLine.head.contains(".BLKW") => parseBlkw2(line)
        case _ if line.isOpCode || line.isDirective => 1.asRight[String]
        case _ =>
          if isLabelLine then
            // two labels in the same line is illegal
            s"ERROR (line ${line.lineNumber.value}): Invalid opcode ('${line.tokenizedLine.head}')".asLeft[Int]
          else
            symbolTable += (line.tokenizedLine.head -> instructionLocation)
            // process the rest of the line after removing the label
            // rest of the line may be empty or not
            if line.tokenizedLine.tail.headOption.exists(!_.startsWith(";")) then
              processLine(line.copy(tokenizedLine = line.tokenizedLine.drop(1)), instructionLocation, true)
            else 0.asRight[String]


    @tailrec
    def loop(lines: List[LineMetadata], instructionLocation: InstructionLocation): Either[String, Unit] =
      lines match
        case Nil => ().asRight[String]
        case firstLine :: remainingLines =>
          instructionsMetadata += InstructionMetadata(firstLine, instructionLocation)
          processLine(firstLine, instructionLocation) match
            case Left(str) => str.asLeft[Unit]
            case Right(nextInstructionLocationIncrement) =>
              loop(remainingLines, instructionLocation ∆+ nextInstructionLocationIncrement)

    loop(linesMetadata, InstructionLocation(0)).map(_ => (instructionsMetadata.toList, symbolTable.toMap))


  def doSyntaxAnalysis(instructionsMetadata: List[InstructionMetadata], symbolTable: Map[String, InstructionLocation]): Either[String, List[Int]] =
    if instructionsMetadata.head.lineMetadata.tokenizedLine.head != ".ORIG" then
      s"ERROR (line ${instructionsMetadata.head.lineMetadata.lineNumber.value}): Instruction not preceeded by a .orig directive".asLeft[List[Int]]
    else
      val l: List[Either[String, List[Int]]] = instructionsMetadata.map { instructionMetadata =>
        val firstToken = instructionMetadata.lineMetadata.tokenizedLine.head
        firstToken match
          case ".ORIG" => parseOrig(instructionMetadata.lineMetadata).map(List(_))
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




