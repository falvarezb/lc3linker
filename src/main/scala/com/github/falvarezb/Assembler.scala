package com.github.falvarezb

import java.io.{FileOutputStream, FileWriter}
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import cats.instances.either
import cats.syntax.either.*
import cats.syntax.all.toTraverseOps
import com.github.falvarezb.ControlInstructions.{parseBr, parseJmp, parseJmpt, parseJsr, parseJsrr, parseTrap}
import com.github.falvarezb.DataMovementInstructions.{parseLd, parseLdi, parseLdr, parseLea, parseSt, parseSti, parseStr}
import com.github.falvarezb.OperateInstructions.{parseAdd, parseAnd, parseNot}
import com.github.falvarezb.Util.parseMemoryAddress
import com.github.falvarezb.Directives.*
import scala.annotation.tailrec
import scala.util.Using

class Assembler:

  protected val symbolTable: mutable.Map[String, InstructionLocation] = mutable.Map.empty

  def link(asmFiles: Seq[String], objFile: String): Either[String, Unit] =
    /**
     * Recursive function that iterates over all files passed as an argument to the linker, on each iteration
     * the instructions' metadata of the file is calculated and accummulated to the result of the previous files.
     *
     * Additionally, the symbol table of each file is created, with each file taking as offset the memory address
     * of the last instruction of the previous file:
     *
     * 1. the location in memory of the first instruction of the first file is given by the ".ORIG" directive.
     *
     * 2. the ".ORIG" directive of the rest of the files is ignored and, instead, the value of instruction counter
     * of the previous file is used as offset for the next file.
     *
     * Returns a list with the instructions' metadata of all files
     *
     * @param asmFiles
     * @param accummulatedInstructionMetadataList
     * @param instructionOffset Value to use instead of .ORIG directive's operand
     * @return
     */
    @tailrec
    def nextFile(asmFiles: Seq[String], accummulatedInstructionMetadataList: List[Either[String, List[InstructionMetadata]]], instructionOffset: Option[InstructionLocation]): List[Either[String, List[InstructionMetadata]]] =
      asmFiles match
        case Nil => accummulatedInstructionMetadataList
        case firstFile :: rest =>
          val fileInstructionMetadataList: Either[String, List[InstructionMetadata]] = for
            linesMetadata <- doLexicalAnalysis(firstFile)
            instructionMetadata <- createSymbolTable(linesMetadata, instructionOffset)
          yield
            instructionOffset match
              case None => instructionMetadata
              case Some(_) => instructionMetadata.drop(1) // drop .ORIG directive
          fileInstructionMetadataList match
            case Left(_) => fileInstructionMetadataList :: accummulatedInstructionMetadataList
            case Right(_) =>
              val nextFileInstructionOffset = fileInstructionMetadataList.map(_.last.instructionLocation).toOption
              nextFile(rest, fileInstructionMetadataList :: accummulatedInstructionMetadataList, nextFileInstructionOffset)

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
      instructions <- doSyntaxAnalysis(instructionMetadataList)
    yield
      val filePrefix = asmFileNamePath.split('.')(0)
      serializeInstructions(instructions, filePrefix)
      serializeSymbolTable(filePrefix)

  private def doLexicalAnalysis(asmFileNamePath: String): Either[String, List[LineMetadata]] =
    Using(Source.fromFile(asmFileNamePath)) { source =>
      source.getLines()
        .map(_.trim)
        .map(line => if line.contains(".STRINGZ") then line else line.takeWhile(_ != ';')) // discard comments in the line
        .zipWithIndex // adding line number before filtering lines
        .filterNot { case (line, _) => line.isEmpty } // removing blank lines
        .map { case (line, idx) => (line, line.split("""[ \t,]""").filterNot(_.isEmpty).toList, idx) } // line tokenization (empty tokens are discarded)
        //.filterNot { case (_, tokenizedLine, _) => tokenizedLine.isEmpty } // removing blank lines
        //.filterNot { case (_, tokenizedLine, _) => tokenizedLine.head.startsWith(";") } // removing comments
        .takeWhile { case (_, tokenizedLine, _) => tokenizedLine.head != ".END" } // discarding lines after '.END' directive
        .map { case (line, tokenizedLine, idx) => LineMetadata(line, tokenizedLine, LineNumber(idx + 1)) }
        .toList
    }.toEither.leftMap(t => s"Error while reading file ${t.getMessage}")


  /**
   * Calculate the instruction location in memory corresponding to each line and with that information create
   * the symbol table. The offset to calculate instructions' address is given by the corresponding parameter.
   * If no offset is provided, the offset is the one given by the '.ORIG' directive
   *
   * Lines corresponding to labels contain the instruction location of the next instruction
   *
   * @param linesMetadata lines and metadata
   * @param instructionOffset
   * @return lines with the instruction location
   */
  private def createSymbolTable(linesMetadata: List[LineMetadata], instructionOffset: Option[InstructionLocation]): Either[String, List[InstructionMetadata]] =

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
     * - directive .ORIG increase instruction counter by the value of its operand unless an instruction offset is
     * provided, in which case the instruction counter is increased by 1
     *
     * As a side effect, found labels are stored in the symbol table
     *
     * @param line                line and metadata
     * @param instructionLocation instruction location corresponding to the given line; needed to build the symbol table
     * @param isLabelLine         flag used to help process lines containing label and instruction at the same time
     * @return distance to the next instruction in memory
     */
    @tailrec
    def processLine(line: LineMetadata, instructionLocation: InstructionLocation, isLabelLine: Boolean = false): Either[String, (Int, Boolean)] =
      val firstToken = line.tokenizedLine.head
      firstToken match
        case ".ORIG" if instructionOffset.isEmpty => parseOrig(line).map((_, isLabelLine))
        case ".ORIG" => 1.asRight[String].map((_, isLabelLine))
        case ".STRINGZ" => stringzAllocatedMemory(line).map((_, isLabelLine))
        case ".BLKW" => blkwAllocatedMemory(line).map((_, isLabelLine))
        case _ if line.isOpCode || line.isDirective => 1.asRight[String].map((_, isLabelLine))
        case label =>
          if isLabelLine then
          // two labels in the same line is illegal
            s"ERROR (line ${line.lineNumber.value}): Invalid opcode ('$label')".asLeft[(Int, Boolean)]
          else
            symbolTable.get(label) match
              case Some(_) =>
                s"ERROR (line ${line.lineNumber.value}): duplicate symbol ('$label')".asLeft[(Int, Boolean)]
              case _ =>
                symbolTable += (label -> instructionLocation)
                line.tokenizedLine.tail match
                  case Nil =>
                    // standalone label, there is nothing else in this line
                    0.asRight[String].map((_, isLabelLine))
                  case tail =>
                    // process the rest of the line after the label
                    // NOTE: if we use Either.cond, processLine won't be tail recursive
                    processLine(line.copy(tokenizedLine = tail), instructionLocation, true)


    @tailrec
    def nextLine(lines: List[LineMetadata], instructions: List[InstructionMetadata], instructionLocation: InstructionLocation): Either[String, List[InstructionMetadata]] =
      lines match
        case Nil => instructions.asRight[String]
        case firstLine :: remainingLines =>
          processLine(firstLine, instructionLocation) match
            case Left(str) => str.asLeft[List[InstructionMetadata]]
            case Right((nextInstructionLocationIncrement, isLabelLine)) =>
              val nextInstructionLocation = instructionLocation ∆+ nextInstructionLocationIncrement
              val updatedLine = if isLabelLine then firstLine.copy(tokenizedLine = firstLine.tokenizedLine.drop(1)) else firstLine
              nextLine(remainingLines, InstructionMetadata(updatedLine, instructionLocation) :: instructions, nextInstructionLocation)

    nextLine(linesMetadata, Nil, instructionOffset.getOrElse(InstructionLocation(0))).map(_.reverse)


  private def doSyntaxAnalysis(instructionsMetadata: List[InstructionMetadata]): Either[String, List[Int]] =
    if instructionsMetadata.head.lineMetadata.tokenizedLine.head != ".ORIG" then
      s"ERROR (line ${instructionsMetadata.head.lineMetadata.lineNumber.value}): Instruction not preceeded by a .orig directive".asLeft[List[Int]]
    else
      val l: List[Either[String, List[Int]]] = instructionsMetadata.map { instructionMetadata =>
        val firstToken = instructionMetadata.lineMetadata.tokenizedLine.head
        firstToken match
          // Directives
          case ".ORIG" => parseOrig(instructionMetadata.lineMetadata).map(List(_))
          case ".STRINGZ" => parseStringz(instructionMetadata.lineMetadata)
          case ".BLKW" => parseBlkw(instructionMetadata.lineMetadata)
          case ".FILL" => parseFill(instructionMetadata.lineMetadata, symbolTable.toMap).map(List(_))
          case "GETC" => List(0xf020).asRight[String]
          case "OUT" => List(0xf021).asRight[String]
          case "PUTS" => List(0xf022).asRight[String]
          case "IN" => List(0xf023).asRight[String]
          case "PUTSP" => List(0xf024).asRight[String]
          case "HALT" => List(0xf025).asRight[String]
          // Operate instructions
          case "ADD" => parseAdd(instructionMetadata.lineMetadata).map(List(_))
          case "AND" => parseAnd(instructionMetadata.lineMetadata).map(List(_))
          case "NOT" => parseNot(instructionMetadata.lineMetadata).map(List(_))
          // Control instructions
          case "JSR" => parseJsr(instructionMetadata, symbolTable.toMap).map(List(_))
          case "JSRR" => parseJsrr(instructionMetadata.lineMetadata).map(List(_))
          case "JMP" => parseJmp(instructionMetadata.lineMetadata).map(List(_))
          case "JMPT" => parseJmpt(instructionMetadata.lineMetadata).map(List(_))
          case "TRAP" => parseTrap(instructionMetadata.lineMetadata).map(List(_))
          case "BRn" => parseBr(instructionMetadata, symbolTable.toMap, ConditionCode.N).map(List(_))
          case "BRz" => parseBr(instructionMetadata, symbolTable.toMap, ConditionCode.Z).map(List(_))
          case "BRp" => parseBr(instructionMetadata, symbolTable.toMap, ConditionCode.P).map(List(_))
          case "BRnz" => parseBr(instructionMetadata, symbolTable.toMap, ConditionCode.NZ).map(List(_))
          case "BRnp" => parseBr(instructionMetadata, symbolTable.toMap, ConditionCode.NP).map(List(_))
          case "BRzp" => parseBr(instructionMetadata, symbolTable.toMap, ConditionCode.ZP).map(List(_))
          case "BRnzp" | "BR" => parseBr(instructionMetadata, symbolTable.toMap, ConditionCode.NZP).map(List(_))
          case "RET" => List(0xc1c0).asRight[String]
          case "RTI" => List(0x8000).asRight[String]
          // Data movement instructions
          case "LD" => parseLd(instructionMetadata, symbolTable.toMap).map(List(_))
          case "LDR" => parseLdr(instructionMetadata, symbolTable.toMap).map(List(_))
          case "LEA" => parseLea(instructionMetadata, symbolTable.toMap).map(List(_))
          case "LDI" => parseLdi(instructionMetadata, symbolTable.toMap).map(List(_))
          case "ST" => parseSt(instructionMetadata, symbolTable.toMap).map(List(_))
          case "STR" => parseStr(instructionMetadata, symbolTable.toMap).map(List(_))
          case "STI" => parseSti(instructionMetadata, symbolTable.toMap).map(List(_))
          // labels and .EXTERN directives come here
          case _ => Nil.asRight[String]
      }
      l.sequence.map(_.flatten)


  private def serializeInstructions(instructions: List[Int], objFileName: String): Unit =
    Using(FileOutputStream(s"$objFileName.obj")) { objFile =>
      instructions.foreach { instr =>
        println(instr.toHexString)
        //JVM's big-endian representation
        //(1 << n) - 1 = 2^n - 1 = 111.. (n times) ..111
        val (mostSignificantByte, leastSignificantByte) = (instr >> 8, instr & ((1 << 8) - 1))
        objFile.write(mostSignificantByte)
        objFile.write(leastSignificantByte)
      }
    }

  private def serializeSymbolTable(objFileName: String): Unit =
    Using(FileWriter(s"$objFileName.sym")) { symFile =>
      symFile.write("// Symbol table\n// Scope level 0:\n//	Symbol Name       Page Address\n//	----------------  ------------\n")
      symbolTable.keys.foreach { key => symFile.write(s"//	$key             ${Integer.toHexString(symbolTable(key).value)}\n")}
    }




