package com.github.falvarezb.lc3linker

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import DataMovementInstructions.*

class DataMovementInstructionsTest extends AnyFunSpec with Matchers:
  describe("LDR parser") {
    it("successful parse when offset is an immediate value") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("LDR", "R0", "R1", "1"), LineNumber(1), ""), InstructionMemoryAddress(0))
      parseLdr(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Right(0x6041)
    }

    it("wrong destination register (DR)") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("LDR", "R8", "R1", "1"), LineNumber(1), "file"), InstructionMemoryAddress(0))
      parseLdr(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Left("ERROR (file - line 1): Expected register but found R8")
    }

    it("wrong base register") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("LDR", "R0", "R8", "1"), LineNumber(1), "file"), InstructionMemoryAddress(0))
      parseLdr(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Left("ERROR (file - line 1): Expected register but found R8")
    }

    it("offset too big") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("LDR", "R0", "R1", "40"), LineNumber(1), "file"), InstructionMemoryAddress(0))
      parseLdr(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Left("ERROR (file - line 1): Immediate operand (40) out of range (-32 to 31)")
    }

    it("offset too small") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("LDR", "R0", "R1", "-40"), LineNumber(1), "file"), InstructionMemoryAddress(0))
      parseLdr(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Left("ERROR (file - line 1): Immediate operand (-40) out of range (-32 to 31)")
    }

    it("symbolic name not found") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("LDR", "R0", "R1", "NON_EXISTENT_LABEL"), LineNumber(1), "file"), InstructionMemoryAddress(0x3001))
      parseLdr(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Left("ERROR (file - line 1): Symbol not found ('NON_EXISTENT_LABEL')")
    }
  }

  describe("STR parser") {
    it("successful parse when offset is an immediate value") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("STR", "R0", "R1", "1"), LineNumber(1), ""), InstructionMemoryAddress(0))
      parseStr(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Right(0x7041)
    }
  }

  describe("LD parser") {
    it("successful parse when offset is an immediate value") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("LD", "R0", "1"), LineNumber(1), ""), InstructionMemoryAddress(0))
      parseLd(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Right(0x2001)
    }

    it("wrong destination register (DR)") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("LD", "R8", "1"), LineNumber(1), "file"), InstructionMemoryAddress(0))
      parseLd(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Left("ERROR (file - line 1): Expected register but found R8")
    }

    it("offset too big") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("LD", "R0", "300"), LineNumber(1), "file"), InstructionMemoryAddress(0))
      parseLd(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Left("ERROR (file - line 1): Immediate operand (300) out of range (-256 to 255)")
    }

    it("offset too small") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("LD", "R0", "-300"), LineNumber(1), "file"), InstructionMemoryAddress(0))
      parseLd(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Left("ERROR (file - line 1): Immediate operand (-300) out of range (-256 to 255)")
    }

    it("symbolic name not found") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("LDR", "R0", "NON_EXISTENT_LABEL"), LineNumber(1), "file"), InstructionMemoryAddress(0x3001))
      parseLd(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Left("ERROR (file - line 1): Symbol not found ('NON_EXISTENT_LABEL')")
    }
  }

  describe("ST parser") {
    it("successful parse when offset is an immediate value") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("ST", "R0", "1"), LineNumber(1), ""), InstructionMemoryAddress(0))
      parseSt(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Right(0x3001)
    }
  }

  describe("LDI parser") {
    it("successful parse when offset is an immediate value") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("LDI", "R0", "1"), LineNumber(1), ""), InstructionMemoryAddress(0))
      parseLdi(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Right(0xa001)
    }
  }

  describe("STI parser") {
    it("successful parse when offset is an immediate value") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("STI", "R0", "1"), LineNumber(1), ""), InstructionMemoryAddress(0))
      parseSti(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Right(0xb001)
    }
  }

  describe("LEA parser") {
    it("successful parse when offset is an immediate value") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("STI", "R0", "1"), LineNumber(1), ""), InstructionMemoryAddress(0))
      parseLea(using instructionMetadata, Map.empty[String, InstructionMemoryAddress]) shouldBe Right(0xe001)
    }
  }
  
