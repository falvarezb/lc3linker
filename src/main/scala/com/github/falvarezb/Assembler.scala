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
    val tokenizedLines = source.getLines().map(_.split(' ').filterNot(_.isEmpty)).filterNot(_.isEmpty).toList

    val instructions = tokenizedLines.map { line =>
      if line(0) == ".ORIG" then 0x0030
      else if line(0) == "ADD" then 0x2110
      else if line(0) == "HALT" then 0x25f0
      else 0
    }.filterNot(_ == 0)

    val objFile = new FileOutputStream("src/test/resources/t1.obj")

    instructions.foreach { instr =>
      println(instr.toHexString)
      //swap bytes (because of little-endian representation)
      val (mostSignificantByte, leastSignificantByte) = (instr >> 8, instr & ((1 << 8) - 1))
      objFile.write(leastSignificantByte + 256)
      objFile.write(mostSignificantByte)
    }
    objFile.close()





