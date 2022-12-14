package com.github.falvarezb.lc3linker

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.io.FileInputStream
import java.util
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

class AssemblerTest extends AnyFunSpec with Matchers :

  def runLinkedFilesTest(asmFileNames: List[String], objFileName: String): Any =
    val path = "src/test/resources"
    val result = new Assembler().link(asmFileNames.map(asmFileName => s"$path/$asmFileName"), s"$path/$objFileName")
    result shouldBe Right(())

    Using(new FileInputStream(s"$path/${objFileName.split('.')(0)}.expected.obj")) { expectedFile =>
      Using(new FileInputStream(s"$path/$objFileName")) { actualFile =>
        util.Arrays.compare(expectedFile.readAllBytes(), actualFile.readAllBytes()) shouldBe 0
      }
    }

  def runAssembledFileTest(asmFileName: String): Unit =
    val path = "src/test/resources"
    val result = new Assembler().assemble(s"$path/$asmFileName")
    result shouldBe Right(())

    val asmFileNameWithoutExtension = asmFileName.split('.')(0)
    Using(new FileInputStream(s"$path/$asmFileNameWithoutExtension.expected.obj")) { expectedFile =>
      Using(new FileInputStream(s"$path/$asmFileNameWithoutExtension.obj")) { actualFile =>
        util.Arrays.compare(expectedFile.readAllBytes(), actualFile.readAllBytes()) shouldBe 0
      }
    }

  def runSymbolTableTest(asmFileName: String) =
    val path = "src/test/resources"
    val symbolTableMock = mutable.HashMap.empty[String, InstructionMemoryAddress]
    val result = new Assembler {
      override protected val symbolTable: mutable.Map[String, InstructionMemoryAddress] = symbolTableMock
    }.assemble(s"$path/$asmFileName")
    (result, symbolTableMock)

  def runSymbolTableSerializationTest(asmFileNames: List[String], objFileName: String) =
    val path = "src/test/resources"
    val result = new Assembler().link(asmFileNames.map(asmFileName => s"$path/$asmFileName"), s"$path/$objFileName")
    result shouldBe Right(())

    Using(Source.fromFile(s"$path/${objFileName.split('.')(0)}.expected.sym")) { expectedFile =>
      Using(Source.fromFile(s"$path/${objFileName.split('.')(0)}.sym")) { actualFile =>
        expectedFile.getLines().toList shouldBe actualFile.getLines().toList
      }
    }

  def runErrorConditionTest(asmFileName: String) =
    val path = "src/test/resources"
    new Assembler().assemble(s"$path/$asmFileName")

  describe("link process") {
    it("day_week + ascii_to_binary") {
      runLinkedFilesTest(List("day_week.asm", "ascii_to_binary_routine.asm"), "day_week.obj")
    }

    it("day_of_week") {
      runLinkedFilesTest(List("day_of_week.asm", "multiplication_routine.asm", "division_routine.asm", "read_multi_digit_routine.asm", "ascii_to_binary_routine.asm"), "day_of_week.obj")
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
      result shouldBe Left("ERROR (src/test/resources/t6.asm - line 4): Immediate operand (#545677767) out of range (0 to 65535)")
    }

    it("asm file without .ORIG directive") {
      val result = runErrorConditionTest("t7.asm")
      result shouldBe Left("ERROR (src/test/resources/t7.asm - line 4): Instruction not preceeded by a .orig directive")
    }

    it("missing .ORIG operand") {
      val result = runErrorConditionTest("t8.asm")
      result shouldBe Left("ERROR (src/test/resources/t8.asm - line 4): Immediate expected")
    }

    it("duplicate label") {
      val result = runErrorConditionTest("t13.asm")
      result shouldBe Left("ERROR (src/test/resources/t13.asm - line 8): duplicate symbol ('LABEL')")
    }

    it("duplicate external label") {
      val result = runErrorConditionTest("t14.asm")
      result shouldBe Left("ERROR (src/test/resources/t14.asm - line 6): duplicate symbol ('LABEL')")
    }
  }

  describe("symbol table") {
    it("label and instruction are in the same line") {
      val (result, symbolTable) = runSymbolTableTest("t3.asm")
      result shouldBe Right(())
      symbolTable should contain("LABEL" -> InstructionMemoryAddress(0x3003))
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
      result shouldBe Left("ERROR (src/test/resources/t5.asm - line 10): Invalid opcode ('LABEL2')")
    }
  }

  describe("symbol table serialization") {

    it("without external symbols") {
      runSymbolTableSerializationTest(List("t2.asm"), "t2.obj")
    }

    it("invalid .ORIG in subroutine") {
      val asmFileNames = List("t2.asm", "t2.asm")
      val path = "src/test/resources"
      val result = new Assembler().link(asmFileNames.map(asmFileName => s"$path/$asmFileName"), s"$path/t2.obj")
      result shouldBe Left("ERROR (src/test/resources/t2.asm - line 4): Invalid .ORIG directive in subroutine")
    }

    it("day_week + ascii_to_binary") {
      runSymbolTableSerializationTest(List("day_week.asm", "ascii_to_binary_routine.asm"), "day_week.obj")
    }
  }


