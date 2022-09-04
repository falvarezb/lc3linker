package com.github.falvarezb

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.io.FileInputStream
import java.util
import scala.collection.mutable
import scala.io.Source

class AssemblerTest extends AnyFunSpec with Matchers:

  def runAssembledFileTest(asmFileName: String): Unit =
    val path = "src/test/resources"
    new Assembler(mutable.HashMap.empty[String, InstructionMemoryAddress]).assemble(s"$path/$asmFileName") shouldBe Right(())

    val asmFileNameWithoutExtension = asmFileName.split('.')(0)
    val expectedFile = new FileInputStream(s"$path/$asmFileNameWithoutExtension.expected.obj")
    val actualFile = new FileInputStream(s"$path/$asmFileNameWithoutExtension.obj")

    util.Arrays.compare(expectedFile.readAllBytes(), actualFile.readAllBytes()) shouldBe 0
    expectedFile.close()
    actualFile.close()

  def runSymbolTableTest(asmFileName: String): mutable.HashMap[String, InstructionMemoryAddress] =
    val path = "src/test/resources"
    val symbolTable = mutable.HashMap.empty[String, InstructionMemoryAddress]
    new Assembler(symbolTable).assemble(s"$path/$asmFileName") shouldBe Right(())
    symbolTable

  describe("assembled file") {
    it("t1: assembly file without labels") {
      runAssembledFileTest("t1.asm")
    }

    it("t2: assembly file with labels") {
      runAssembledFileTest("t2.asm")
    }
  }

  describe("symbol table") {
    it("label and instruction are in the same line") {
      val symbolTable = runSymbolTableTest("t3.asm")
      symbolTable should contain ("LABEL" -> InstructionMemoryAddress(0x3003))
    }

    it("multiple labels associated to the same instruction") {
      val symbolTable = runSymbolTableTest("t4.asm")
      symbolTable should contain("LABEL1" -> InstructionMemoryAddress(0x3003))
      symbolTable should contain("LABEL2" -> InstructionMemoryAddress(0x3001))
      symbolTable should contain("LABEL3" -> InstructionMemoryAddress(0x3002))
      symbolTable should contain("LABEL4" -> InstructionMemoryAddress(0x3004))
      symbolTable should contain("LABEL5" -> InstructionMemoryAddress(0x3003))
    }
  }


