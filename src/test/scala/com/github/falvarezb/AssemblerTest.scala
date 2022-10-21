package com.github.falvarezb

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.io.FileInputStream
import java.util
import scala.collection.mutable
import scala.io.Source

class AssemblerTest extends AnyFunSpec with Matchers :

  def runLinkedFilesTest(asmFileNames: List[String], objFileName: String): Any =
    val path = "src/test/resources"
    val result = new Assembler().link(asmFileNames.map(asmFileName => s"$path/$asmFileName"), s"$path/$objFileName")
    result shouldBe Right(())

    val expectedFile = new FileInputStream(s"$path/${objFileName.split('.')(0)}.expected.obj")
    val actualFile = new FileInputStream(s"$path/$objFileName")

    util.Arrays.compare(expectedFile.readAllBytes(), actualFile.readAllBytes()) shouldBe 0
    expectedFile.close()
    actualFile.close()

  def runAssembledFileTest(asmFileName: String): Unit =
    val path = "src/test/resources"
    val result = new Assembler().assemble(s"$path/$asmFileName")
    result shouldBe Right(())

    val asmFileNameWithoutExtension = asmFileName.split('.')(0)
    val expectedFile = new FileInputStream(s"$path/$asmFileNameWithoutExtension.expected.obj")
    val actualFile = new FileInputStream(s"$path/$asmFileNameWithoutExtension.obj")

    util.Arrays.compare(expectedFile.readAllBytes(), actualFile.readAllBytes()) shouldBe 0
    expectedFile.close()
    actualFile.close()

  def runSymbolTableTest(asmFileName: String) =
    val path = "src/test/resources"
    val symbolTableMock = mutable.HashMap.empty[String, InstructionLocation]
    val result = new Assembler {
      override protected val symbolTable: mutable.Map[String, InstructionLocation] = symbolTableMock
    }.assemble(s"$path/$asmFileName")
    (result, symbolTableMock)

  def runSymbolTableSerializationTest(asmFileNames: List[String], objFileName: String) =
    val path = "src/test/resources"
    val symbolTableMock = mutable.HashMap.empty[String, InstructionLocation]
    val result = new Assembler {
      override protected val symbolTable: mutable.Map[String, InstructionLocation] = symbolTableMock
    }.link(asmFileNames.map(asmFileName => s"$path/$asmFileName"), s"$path/$objFileName")
    result shouldBe Right(())

    val expectedFile = Source.fromFile(s"$path/${objFileName.split('.')(0)}.expected.sym")
    val actualFile = Source.fromFile(s"$path/${objFileName.split('.')(0)}.sym")

    expectedFile.getLines().toList shouldBe actualFile.getLines().toList
    expectedFile.close()
    actualFile.close()

  def runErrorConditionTest(asmFileName: String) =
    val path = "src/test/resources"
    new Assembler().assemble(s"$path/$asmFileName")

  describe("link process") {
    it("linking without symbols") {
      runLinkedFilesTest(List("t1.asm", "t1.asm"), "t1_t1.obj")
    }

    it("linking with symbols") {
      runLinkedFilesTest(List("t2.asm", "t4.asm"), "t2_t4.obj")
    }

    it("linking external symbols") {
      runLinkedFilesTest(List("t2_external.asm", "t2.asm"), "t2_external.obj")
    }
  }

  describe("assembly process") {
    it("t1: assembly file without labels") {
      runAssembledFileTest("t1.asm")
    }

    it("t2: assembly file with labels") {
      runAssembledFileTest("t2.asm")
    }

    it("t3: label and instruction are in the same line") {
      runAssembledFileTest("t3.asm")
    }

    it("t9: .STRINGZ directive") {
      runAssembledFileTest("t9.asm")
    }

    it("t10: .BLKW directive") {
      runAssembledFileTest("t10.asm")
    }

    it("t11: backwards jump") {
      runAssembledFileTest("t11.asm")
    }

    it("abs") {
      runAssembledFileTest("abs.asm")
    }

    it("charcounter") {
      runAssembledFileTest("charcounter.asm")
    }

    it("lcrng") {
      runAssembledFileTest("lcrng.asm")
    }

    it("or") {
      runAssembledFileTest("or.asm")
    }

    it("lc3os") {
      runAssembledFileTest("lc3os.asm")
    }

    it("2048") {
      runAssembledFileTest("2048.asm")
    }
  }

  describe("error conditions") {
    it("wrong .ORIG operand") {
      val result = runErrorConditionTest("t6.asm")
      result shouldBe Left("ERROR (line 4): Immediate operand (#545677767) out of range (0 to 65535)")
    }

    it("asm file without .ORIG directive") {
      val result = runErrorConditionTest("t7.asm")
      result shouldBe Left("ERROR (line 4): Instruction not preceeded by a .orig directive")
    }

    it("missing .ORIG operand") {
      val result = runErrorConditionTest("t8.asm")
      result shouldBe Left("ERROR (line 4): Immediate expected")
    }

    it("duplicate label") {
      val result = runErrorConditionTest("t13.asm")
      result shouldBe Left("ERROR (line 8): duplicate symbol ('LABEL')")
    }

    it("duplicate external label") {
      val result = runErrorConditionTest("t14.asm")
      result shouldBe Left("ERROR (line 6): duplicate symbol ('LABEL')")
    }
  }

  describe("symbol table") {
    it("label and instruction are in the same line") {
      val (result, symbolTable) = runSymbolTableTest("t3.asm")
      result shouldBe Right(())
      symbolTable should contain("LABEL" -> InstructionLocation(0x3003))
    }

    it("multiple labels associated to the same instruction") {
      val (result, symbolTable) = runSymbolTableTest("t4.asm")
      result shouldBe Right(())
      symbolTable should contain("LABEL1" -> InstructionLocation(0x3003))
      symbolTable should contain("LABEL2" -> InstructionLocation(0x3001))
      symbolTable should contain("LABEL3" -> InstructionLocation(0x3002))
      symbolTable should contain("LABEL4" -> InstructionLocation(0x3004))
      symbolTable should contain("LABEL5" -> InstructionLocation(0x3003))
    }

    it("two labels in the same line is illegal") {
      val (result, _) = runSymbolTableTest("t5.asm")
      result shouldBe Left("ERROR (line 10): Invalid opcode ('LABEL2')")
    }
  }

  describe("symbol table serialization") {

    it("without external symbols") {
      runSymbolTableSerializationTest(List("t2.asm"), "t2.obj")
    }

    it("with external symbols") {
      runSymbolTableSerializationTest(List("t2_external.asm", "t2.asm"), "t2_external.obj")
    }
  }


