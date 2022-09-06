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

  def runSymbolTableTest(asmFileName: String) =
    val path = "src/test/resources"
    val symbolTable = mutable.HashMap.empty[String, InstructionMemoryAddress]
    val result = new Assembler(symbolTable).assemble(s"$path/$asmFileName")
    (result, symbolTable)

  def runErrorConditionTest(asmFileName: String) =
    val path = "src/test/resources"
    val symbolTable = mutable.HashMap.empty[String, InstructionMemoryAddress]
    new Assembler(symbolTable).assemble(s"$path/$asmFileName")

  describe("assembled file") {
    it("t1: assembly file without labels") {
      runAssembledFileTest("t1.asm")
    }

    it("t2: assembly file with labels") {
      runAssembledFileTest("t2.asm")
    }

    it("t9: .STRINGZ directive") {
      runAssembledFileTest("t9.asm")
    }
  }

  describe("error conditions") {
    it("wrong .ORIG operand") {
      val result = runErrorConditionTest("t6.asm")
      result shouldBe Left("ERROR (line 4): Immediate operand (545677767) outside of range (0 to 65535)")
    }

    it("asm file without .ORIG directive") {
      val result = runErrorConditionTest("t7.asm")
      result shouldBe Left("ERROR (line 4): Instruction not preceeded by a .orig directive")
    }

    it("missing .ORIG operand") {
      val result = runErrorConditionTest("t8.asm")
      result shouldBe Left("ERROR (line 4): Immediate expected")
    }
  }

  describe("symbol table") {
    it("label and instruction are in the same line") {
      val (result, symbolTable) = runSymbolTableTest("t3.asm")
      result shouldBe Right(())
      symbolTable should contain ("LABEL" -> InstructionMemoryAddress(0x3003))
    }

    it("multiple labels associated to the same instruction") {
      val (result, symbolTable) = runSymbolTableTest("t4.asm")
      result shouldBe Right(())
      symbolTable should contain("LABEL1" -> InstructionMemoryAddress(0x3003))
      symbolTable should contain("LABEL2" -> InstructionMemoryAddress(0x3001))
      symbolTable should contain("LABEL3" -> InstructionMemoryAddress(0x3002))
      symbolTable should contain("LABEL4" -> InstructionMemoryAddress(0x3004))
      symbolTable should contain("LABEL5" -> InstructionMemoryAddress(0x3003))
    }

    it("two labels in the same line is illegal") {
      val (result, _) = runSymbolTableTest("t5.asm")
      result shouldBe Left("ERROR (line 10): Invalid opcode ('LABEL2')")
    }
  }


