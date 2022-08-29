package com.github.falvarezb

import java.io.FileOutputStream
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Assembler:

  def assemble(asmFileNamePath: String): Either[String, Unit] =
    val instructionsMetadata = mutable.ListBuffer.empty[InstructionMetadata]
    val symbolTable = mutable.HashMap.empty[String, InstructionNumber]
    val linesMetadata = doLexicalAnalysis(asmFileNamePath)
    createSymbolTable(linesMetadata, instructionsMetadata, InstructionNumber(0), symbolTable)
    val instructions = doSyntaxAnalysis(instructionsMetadata, symbolTable)
    write_obj(instructions, asmFileNamePath)
    Right(())


  def doLexicalAnalysis(asmFileNamePath: String)=
    val source = Source.fromFile(asmFileNamePath)
    val tokenizedLinesIter = source.getLines().map(_.split("[ ,]").filterNot(_.isEmpty)).filterNot(_.isEmpty).zipWithIndex.map{
      case (tokenizedLine, idx) => LineMetadata(tokenizedLine, LineNumber(idx))}
    val tokenizedLines = ListBuffer[LineMetadata]()
    var endFound = false;
    while !endFound && tokenizedLinesIter.hasNext do
      val next = tokenizedLinesIter.next()
      if next.tokenizedLine(0) == ".END" then endFound = true
      else tokenizedLines += next
    tokenizedLines.toList

  def createSymbolTable(linesMetadata: Seq[LineMetadata], instructionsMetadata: mutable.ListBuffer[InstructionMetadata], instructionNumber: InstructionNumber, symbolTable: mutable.HashMap[String, InstructionNumber]): mutable.Map[String, InstructionNumber] =
    linesMetadata match
      case Nil => symbolTable
      case x :: xs => x match
        case lineMetadata if lineMetadata.tokenizedLine(0) == ".ORIG" => createSymbolTable(xs, instructionsMetadata += InstructionMetadata(lineMetadata, instructionNumber), InstructionNumber(parseOrig(lineMetadata.tokenizedLine)), symbolTable)
        case lineMetadata if lineMetadata.isOpCode || lineMetadata.isDirective || lineMetadata.isComment => createSymbolTable(xs, instructionsMetadata += InstructionMetadata(lineMetadata, instructionNumber + 1), instructionNumber + 1, symbolTable)
        case lineMetadata => createSymbolTable(xs, instructionsMetadata, instructionNumber, symbolTable += (lineMetadata.tokenizedLine(0) -> instructionNumber))

  def doSyntaxAnalysis(instructionsMetadata: mutable.Seq[InstructionMetadata], symbolTable: mutable.Map[String, InstructionNumber]): mutable.Seq[Int] =
    instructionsMetadata.map {
      case instructionMetadata if instructionMetadata.lineMetadata.tokenizedLine(0) == ".ORIG" => Some(parseOrig(instructionMetadata.lineMetadata.tokenizedLine))
      case instructionMetadata if instructionMetadata.lineMetadata.tokenizedLine(0) == "ADD" => Some(parseAdd(instructionMetadata.lineMetadata.tokenizedLine))
      case instructionMetadata if instructionMetadata.lineMetadata.tokenizedLine(0) == "JSR" => Some(parseJsr(instructionMetadata, symbolTable))
      case instructionMetadata if instructionMetadata.lineMetadata.tokenizedLine(0) == "HALT" => Some(0xf025)
      case _ => None
    }.filterNot(_.isEmpty).map(_.get)

  def write_obj(instructions: mutable.Seq[Int], asmFileNamePath: String): Unit =
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

  def parseOrig(tokens: Array[String]): Int =
    Integer.parseInt(tokens(1).drop(1), 16)

  def parseJsr(instructionMetadata: InstructionMetadata, symbolTable: mutable.Map[String, InstructionNumber]): Int =
    val label = instructionMetadata.lineMetadata.tokenizedLine(1)
    val offset =  symbolTable(label).value - instructionMetadata.instructionNumber.value - 1
    (4 << 12) +
      (1 << 11) +
      offset

  def parseAdd(tokens: Array[String]): Int =
    val immediateBit = if tokens(3)(0) == 'R' then 0 else 1 << 5
    //ops code: 0001
    (1 << 12) +
      (tokens(1).substring(1).toInt << 9) +
      (tokens(2).substring(1).toInt << 6) +
      immediateBit +
      tokens(3).substring(1).toInt


