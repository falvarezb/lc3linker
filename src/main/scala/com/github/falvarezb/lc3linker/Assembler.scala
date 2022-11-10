package com.github.falvarezb.lc3linker

import java.io.{FileOutputStream, FileWriter}
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import cats.instances.either
import cats.syntax.either.*
import cats.syntax.all.toTraverseOps
import ControlInstructions.{parseBr, parseJmp, parseJmpt, parseJsr, parseJsrr, parseTrap}
import DataMovementInstructions.{parseLd, parseLdi, parseLdr, parseLea, parseSt, parseSti, parseStr}
import OperateInstructions.{parseAdd, parseAnd, parseNot}
import Util.parseMemoryAddress
import Directives.*
import scala.annotation.tailrec
import scala.util.Using

class Assembler:

  protected val symbolTable: mutable.Map[String, InstructionMemoryAddress] = mutable.Map.empty

  def link(asmFiles: Seq[String], objFile: String): Either[String, Unit] =
    /**
     * Recursive function that iterates over all asm files passed as an argument to the linker, on each iteration
     * the instructions' metadata of that file is calculated and accummulated to the result of the previous files.
     *
     * Additionally, the symbol table of each file is created, with each file taking as offset the memory address
     * of the last instruction of the previous file:
     *
     * 1. the location in memory of the first instruction of the first file is given by the ".ORIG" directive.
     *
     * 2. the rest of the files must not have an .ORIG directive, instead, the value of instruction counter
     * of the previous file is used as offset for the next file.
     *
     * Returns a list with the instructions' metadata of all files
     *
     * @param asmFiles
     * @param accummulatedInstructionMetadataList
     * @param fileInstructionOffset Value to use instead of .ORIG directive's operand
     * @return
     */
    @tailrec
    def nextFile(asmFiles: Seq[String], accummulatedInstructionMetadataList: List[Either[String, List[InstructionMetadata]]], fileInstructionOffset: Option[InstructionMemoryAddress]): List[Either[String, List[InstructionMetadata]]] =
      asmFiles match
        case Nil => accummulatedInstructionMetadataList
        case firstFile :: rest =>
          doLexicalAnalysis(firstFile).flatMap { linesMetadata =>
            createSymbolTable(linesMetadata, fileInstructionOffset)
          } match
            case Left(str) => List(str.asLeft[List[InstructionMetadata]])
            case Right((fileInstructionMetadataList, nextFileInstructionOffset)) => nextFile(rest, fileInstructionMetadataList.asRight[String] :: accummulatedInstructionMetadataList, Some(nextFileInstructionOffset))

    for
      instructionMetadataList <- nextFile(asmFiles, Nil, None).reverse.sequence.map(_.flatten)
      instructions <- doSyntaxAnalysis(instructionMetadataList)
    yield
      serializeInstructions(instructions, objFile)
      serializeSymbolTable(objFile)

  def assemble(asmFileNamePath: String): Either[String, Unit] =
    for
      linesMetadata <- doLexicalAnalysis(asmFileNamePath)
      instructionMetadataList <- createSymbolTable(linesMetadata, None)
      instructions <- doSyntaxAnalysis(instructionMetadataList._1)
    yield
      val asmFileNameWithoutExtension = asmFileNamePath.split('.')(0)
      serializeInstructions(instructions, s"$asmFileNameWithoutExtension.obj")
      serializeSymbolTable(s"$asmFileNameWithoutExtension.sym")

  private def doLexicalAnalysis(asmFileNamePath: String): Either[String, List[LineMetadata]] =
    Using(Source.fromFile(asmFileNamePath)) { source =>
      source.getLines()
        .map(_.trim)
        .map(line => if line.contains(".STRINGZ") then line else line.takeWhile(_ != ';')) // discard comments in the line
        .zipWithIndex // adding line number before filtering lines
        .filterNot { case (line, _) => line.isEmpty } // removing blank lines
        .map { case (line, idx) => (line, line.split("""[ \t,]""").filterNot(_.isEmpty).toList, idx) } // line tokenization (empty tokens are discarded)
        .takeWhile { case (_, tokenizedLine, _) => tokenizedLine.head != ".END" } // discarding lines after '.END' directive
        .map { case (line, tokenizedLine, idx) => LineMetadata(line, tokenizedLine, LineNumber(idx + 1), asmFileNamePath) }
        .toList
    }.toEither.leftMap(t => s"Error while reading file ${t.getMessage}")


  /**
   * Calculate the memory address corresponding to each line and with that information create
   * the symbol table. The offset to calculate instructions' address is given by the corresponding parameter.
   * If no offset is provided, the offset is the one given by the '.ORIG' directive
   *
   * Lines corresponding to labels contain the memory adress of the next instruction
   *
   * @param linesMetadata lines and metadata
   * @param instructionOffset
   * @return tuple consisting in the lines with the memory address and the first free memory address after the
   *         last instruction
   */
  private def createSymbolTable(linesMetadata: List[LineMetadata], instructionOffset: Option[InstructionMemoryAddress]): Either[String, (List[InstructionMetadata], InstructionMemoryAddress)] =

    /**
     * Process given line to calculate and return the distance to the next instruction in memory, e.g.
     *
     * - comments, labels and directive .END do not increment instruction counter
     *
     * - instructions increase instruction counter by 1
     *
     * - directive .BLKW increases instruction counter by the number of words that needs to allocate
     *
     * - directive .STRINGZ increase instruction counter by the number of chars of the corresponding string
     *
     * - directive .ORIG increase instruction counter by the value of its operand
     *
     * As a side effect, found labels are stored in the symbol table
     *
     * @param line                line and metadata
     * @param instructionMemoryAddress memory address of the instruction corresponding to the given line; needed to build the symbol table
     * @param isLabelLine         flag used to help process lines containing both label and instruction at the same time
     * @return tuple consisting of the distance to the next instruction in memory and a boolean to indicate whether the
     *         line follows a label in the same line
     */
    @tailrec
    def processLine(instructionMemoryAddress: InstructionMemoryAddress, isLabelLine: Boolean = false)(using line: LineMetadata): Either[String, (Int, Boolean)] =
      val firstToken = line.tokenizedLine.head
      val lineNumber = line.lineNumber
      val fileName = line.fileName
      firstToken match
        case ".ORIG" if instructionOffset.isEmpty => parseOrig.map((_, isLabelLine))
        case ".ORIG" => s"ERROR ($fileName - line ${lineNumber.value}): Invalid .ORIG directive in subroutine".asLeft[(Int, Boolean)]
        case ".STRINGZ" => stringzAllocatedMemory.map((_, isLabelLine))
        case ".BLKW" => blkwAllocatedMemory.map((_, isLabelLine))
        case _ if line.isOpCode || line.isDirective => 1.asRight[String].map((_, isLabelLine))
        case label =>
          if isLabelLine then
          // two labels in the same line is illegal
            s"ERROR ($fileName - line ${lineNumber.value}): Invalid opcode ('$label')".asLeft[(Int, Boolean)]
          else
            symbolTable.get(label) match
              case Some(_) =>
                s"ERROR ($fileName - line ${lineNumber.value}): duplicate symbol ('$label')".asLeft[(Int, Boolean)]
              case _ =>
                symbolTable += (label -> instructionMemoryAddress)
                line.tokenizedLine.tail match
                  case Nil =>
                    // standalone label, there is nothing else in this line
                    0.asRight[String].map((_, isLabelLine))
                  case tail =>
                    // process the rest of the line after the label
                    // NOTE: if we use Either.cond, processLine won't be tail recursive
                    processLine(instructionMemoryAddress, true)(using line.copy(tokenizedLine = tail))


    @tailrec
    def nextLine(lines: List[LineMetadata], instructions: List[InstructionMetadata], instructionMemoryAddress: InstructionMemoryAddress): Either[String, (List[InstructionMetadata], InstructionMemoryAddress)] =
      lines match
        case Nil => (instructions, instructionMemoryAddress).asRight[String]
        case firstLine :: remainingLines =>
          processLine(instructionMemoryAddress)(using firstLine) match
            case Left(str) => str.asLeft[(List[InstructionMetadata], InstructionMemoryAddress)]
            case Right((nextInstructionLocationIncrement, isLabelLine)) =>
              val nextInstructionLocation = instructionMemoryAddress ∆+ nextInstructionLocationIncrement
              val updatedLine = if isLabelLine then firstLine.copy(tokenizedLine = firstLine.tokenizedLine.drop(1)) else firstLine
              nextLine(remainingLines, InstructionMetadata(updatedLine, instructionMemoryAddress) :: instructions, nextInstructionLocation)

    nextLine(linesMetadata, Nil, instructionOffset.getOrElse(InstructionMemoryAddress(0))).map { case (instructionMetadataList, nextInstructionLocation) =>
      (instructionMetadataList.reverse, nextInstructionLocation)
    }


  private def doSyntaxAnalysis(instructionsMetadata: List[InstructionMetadata]): Either[String, List[Int]] =
    if instructionsMetadata.head.lineMetadata.tokenizedLine.head != ".ORIG" then
      s"ERROR (${instructionsMetadata.head.lineMetadata.fileName} - line ${instructionsMetadata.head.lineMetadata.lineNumber.value}): Instruction not preceeded by a .orig directive".asLeft[List[Int]]
    else
      val l: List[Either[String, List[Int]]] = instructionsMetadata.map { instructionMetadata =>
        given lineMetadata: LineMetadata = instructionMetadata.lineMetadata
        given givenInstructionMetadata: InstructionMetadata = instructionMetadata
        given givenSymbolTable: SymbolTable = symbolTable.toMap
        val firstToken = instructionMetadata.lineMetadata.tokenizedLine.head
        val instructions: Either[String, Int | List[Int]] = firstToken match
          // Directives
          case ".ORIG" => parseOrig
          case ".STRINGZ" => parseStringz
          case ".BLKW" => parseBlkw
          case ".FILL" => parseFill
          case "GETC" => Right(0xf020)
          case "OUT" => Right(0xf021)
          case "PUTS" => Right(0xf022)
          case "IN" => Right(0xf023)
          case "PUTSP" => Right(0xf024)
          case "HALT" => Right(0xf025)
          // Operate instructions
          case "ADD" => parseAdd
          case "AND" => parseAnd
          case "NOT" => parseNot
          // Control instructions
          case "JSR" => parseJsr
          case "JSRR" => parseJsrr
          case "JMP" => parseJmp
          case "JMPT" => parseJmpt
          case "TRAP" => parseTrap
          case "BRn" => parseBr(ConditionCode.N)
          case "BRz" => parseBr(ConditionCode.Z)
          case "BRp" => parseBr(ConditionCode.P)
          case "BRnz" => parseBr(ConditionCode.NZ)
          case "BRnp" => parseBr(ConditionCode.NP)
          case "BRzp" => parseBr(ConditionCode.ZP)
          case "BRnzp" | "BR" => parseBr(ConditionCode.NZP)
          case "RET" => Right(0xc1c0)
          case "RTT" => Right(0xc1c1)
          case "RTI" => Right(0x8000)
          // Data movement instructions
          case "LD" => parseLd
          case "LDR" => parseLdr
          case "LEA" => parseLea
          case "LDI" => parseLdi
          case "ST" => parseSt
          case "STR" => parseStr
          case "STI" => parseSti
          // labels come here
          case _ => Right(Nil)

        instructions.map {
          case x: Int => List(x)
          case x: List[Int] => x
        }
      }
      l.sequence.map(_.flatten)


  private def serializeInstructions(instructions: List[Int], objFileName: String): Unit =
    Using(FileOutputStream(s"$objFileName")) { objFile =>
      instructions.foreach { instr =>
        //JVM's big-endian representation
        //(1 << n) - 1 = 2^n - 1 = 111.. (n times) ..111
        val (mostSignificantByte, leastSignificantByte) = (instr >> 8, instr & ((1 << 8) - 1))
        objFile.write(mostSignificantByte)
        objFile.write(leastSignificantByte)
      }
    }

  private def serializeSymbolTable(objFileName: String): Unit =
    Using(FileWriter(s"${objFileName.split('.')(0)}.sym")) { symFile =>
      symFile.write("// Symbol table\n// Scope level 0:\n//	Symbol Name       Page Address\n//	----------------  ------------\n")
      symbolTable.keys.foreach { key => symFile.write(s"//	$key             ${Integer.toHexString(symbolTable(key).value)}\n") }
    }




