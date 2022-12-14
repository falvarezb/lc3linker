package com.github.falvarezb.lc3linker

import ControlInstructions.*
import OpCode.BR
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ControlInstructionsTest extends AnyFunSpec with Matchers:
  describe("JSR parser") {
    it("successful parse when operand is an immediate value in decimal representation") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("JSR", "#1"), LineNumber(1), ""), InstructionMemoryAddress(0))
      parseJsr(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Right(0x4801)
    }

    it("successful parse when operand is an immediate value in default decimal representation") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("JSR", "1"), LineNumber(1), ""), InstructionMemoryAddress(0))
      parseJsr(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Right(0x4801)
    }

    it("successful parse when operand is an immediate value in hex representation") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("JSR", "xa"), LineNumber(1), ""), InstructionMemoryAddress(0))
      parseJsr(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Right(0x480a)
    }

    it("successful parse when operand is a symbolic name") {
      val symbolicTable = Map("LABEL" -> InstructionMemoryAddress(0x3003))
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("JSR", "LABEL"), LineNumber(1), ""), InstructionMemoryAddress(0x3001))
      parseJsr(using instructionMetadata, symbolicTable) shouldBe Right(0x4801)
    }

    it("immediate too big") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("JSR", "2000"), LineNumber(1), "file"), InstructionMemoryAddress(0))
      parseJsr(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Left("ERROR (file - line 1): Immediate operand (2000) out of range (-1024 to 1023)")
    }

    it("immediate too small") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("JSR", "-2000"), LineNumber(1), "file"), InstructionMemoryAddress(0))
      parseJsr(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Left("ERROR (file - line 1): Immediate operand (-2000) out of range (-1024 to 1023)")
    }

    it("symbolic name not found") {
      val instructionMetadata: InstructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("JSR", "NON_EXISTENT_LABEL"), LineNumber(1), "file"), InstructionMemoryAddress(0))
      parseJsr(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Left("ERROR (file - line 1): Symbol not found ('NON_EXISTENT_LABEL')")
    }
  }

  describe("JSRR parser") {
    it("successful parse") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("JSRR", "R0"), LineNumber(1), "")
      parseJsrr shouldBe Right(0x4000)
    }

    it("base register is wrong") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("JSRR", "R8"), LineNumber(1), "file")
      parseJsrr shouldBe Left("ERROR (file - line 1): Expected register but found R8")
    }
  }

  describe("JMP parser") {
    it("successful parse") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("JMP", "R6"), LineNumber(1), "")
      parseJmp shouldBe Right(0xC180)
    }

    it("base register is wrong") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("JMP", "R8"), LineNumber(1), "file")
      parseJmp shouldBe Left("ERROR (file - line 1): Expected register but found R8")
    }
  }

  describe("BR parser") {
    it("successful BR/BRnzp") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("BR", "1"), LineNumber(1), ""), InstructionMemoryAddress(0))
      parseBr(ConditionCode.NZP)(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Right(0x0e01)
    }

    it("successful BRp") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("BRp", "1"), LineNumber(1), ""), InstructionMemoryAddress(0))
      parseBr(ConditionCode.P)(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Right(0x0201)
    }

    it("successful BRz") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("BRz", "1"), LineNumber(1), ""), InstructionMemoryAddress(0))
      parseBr(ConditionCode.Z)(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Right(0x0401)
    }

    it("successful BRn") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("BRn", "1"), LineNumber(1), ""), InstructionMemoryAddress(0))
      parseBr(ConditionCode.N)(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Right(0x0801)
    }

    it("successful BRzp") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("BRzp", "1"), LineNumber(1), ""), InstructionMemoryAddress(0))
      parseBr(ConditionCode.ZP)(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Right(0x0601)
    }

    it("successful BRnp") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("BRnp", "1"), LineNumber(1), ""), InstructionMemoryAddress(0))
      parseBr(ConditionCode.NP)(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Right(0x0a01)
    }

    it("successful BRnz") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("BRnz", "1"), LineNumber(1), ""), InstructionMemoryAddress(0))
      parseBr(ConditionCode.NZ)(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Right(0x0c01)
    }

    it("offset too big") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("BR", "300"), LineNumber(1), "file"), InstructionMemoryAddress(0))
      parseBr(ConditionCode.NZP)(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Left("ERROR (file - line 1): Immediate operand (300) out of range (-256 to 255)")
    }

    it("offset too small") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("BR", "-300"), LineNumber(1), "file"), InstructionMemoryAddress(0))
      parseBr(ConditionCode.NZP)(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Left("ERROR (file - line 1): Immediate operand (-300) out of range (-256 to 255)")
    }

    it("successful parse when operand is a symbolic name") {
      val symbolTable = Map("LABEL" -> InstructionMemoryAddress(0x3003))
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("BR", "LABEL"), LineNumber(1), ""), InstructionMemoryAddress(0x3001))
      parseBr(ConditionCode.NZP)(using instructionMetadata, symbolTable) shouldBe Right(0x0e01)
    }

    it("symbolic name not found") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("BR", "NON_EXISTENT_LABEL"), LineNumber(1), "file"), InstructionMemoryAddress(0x3001))
      parseBr(ConditionCode.NZP)(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Left("ERROR (file - line 1): Symbol not found ('NON_EXISTENT_LABEL')")
    }
  }

  describe("TRAP parser") {
    it("successful parse") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("TRAP", "1"), LineNumber(1), "")
      parseTrap shouldBe Right(0xf001)
    }

    it("trapvector too big") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("TRAP", "300"), LineNumber(1), "file")
      parseTrap shouldBe Left("ERROR (file - line 1): Immediate operand (300) out of range (0 to 255)")
    }

    it("trapvector too small") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("TRAP", "-1"), LineNumber(1), "file")
      parseTrap shouldBe Left("ERROR (file - line 1): Immediate operand (-1) out of range (0 to 255)")
    }
  }
