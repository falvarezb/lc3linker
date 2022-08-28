package com.github.falvarezb

import java.io.FileOutputStream
import scala.collection.mutable
import scala.io.Source

object Assembler:

  val symbolTable = mutable.HashMap[String, Int]()



  def assemble(asmFileNamePath: String): Either[String,Unit] =
    doLexicalAnalysis(asmFileNamePath)
    Right(())


  def doLexicalAnalysis(asmFileNamePath: String) =
    val source = Source.fromFile(asmFileNamePath)
    val tokenizedLines = source.getLines().map(_.split("[ ,]").filterNot(_.isEmpty)).filterNot(_.isEmpty).toList

    val instructions = tokenizedLines.map { lineTokens =>
      if lineTokens(0) == ".ORIG" then parseOrig(lineTokens)
      else if lineTokens(0) == "ADD" then parseAdd(lineTokens)
      else if lineTokens(0) == "HALT" then 0xf025
      else 0
    }.filterNot(_ == 0)

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


  def parseOrig(line: Array[String]): Int =
    Integer.parseInt(line(1).drop(1), 16)

  def parseAdd(tokens: Array[String]): Int =
    //ops code: 0001
    (1 << 12) +
    (tokens(1).substring(1).toInt << 9) +
    (tokens(2).substring(1).toInt << 6) +
    (1 << 5) +
    tokens(3).substring(1).toInt


