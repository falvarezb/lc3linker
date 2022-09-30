package com.github.falvarezb

import com.github.falvarezb.ControlInstructions.{parseJmp, parseJsr, parseJsrr}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ControlInstructionsTest extends AnyFunSpec with Matchers:
  describe("JSR parser") {
    it("successful parse when operand is an immediate value in decimal representation") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("JSR", "#1"), LineNumber(1)), InstructionLocation(0))
      parseJsr(instructionMetadata, Map.empty[String, InstructionLocation]) shouldBe Right(0x4801)
    }

    it("successful parse when operand is an immediate value in default decimal representation") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("JSR", "1"), LineNumber(1)), InstructionLocation(0))
      parseJsr(instructionMetadata, Map.empty[String, InstructionLocation]) shouldBe Right(0x4801)
    }

    it("successful parse when operand is an immediate value in hex representation") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("JSR", "xa"), LineNumber(1)), InstructionLocation(0))
      parseJsr(instructionMetadata, Map.empty[String, InstructionLocation]) shouldBe Right(0x480a)
    }

    it("successful parse when operand is a symbolic name") {
      val symbolicTable = Map("LABEL" -> InstructionLocation(0x3003))
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("JSR", "LABEL"), LineNumber(1)), InstructionLocation(0x3001))
      parseJsr(instructionMetadata, symbolicTable) shouldBe Right(0x4801)
    }

    it("immediate too big") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("JSR", "2000"), LineNumber(1)), InstructionLocation(0))
      parseJsr(instructionMetadata, Map.empty[String, InstructionLocation]) shouldBe Left("ERROR (line 1): Immediate operand (2000) out of range (-1024 to 1023)")
    }

    it("immediate too small") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("JSR", "-2000"), LineNumber(1)), InstructionLocation(0))
      parseJsr(instructionMetadata, Map.empty[String, InstructionLocation]) shouldBe Left("ERROR (line 1): Immediate operand (-2000) out of range (-1024 to 1023)")
    }

    it("symbolic name not found") {
      val instructionMetadata = InstructionMetadata(LineMetadata("DOES NOT MATTER", List("JSR", "NON_EXISTENT_LABEL"), LineNumber(1)), InstructionLocation(0))
      parseJsr(instructionMetadata, Map.empty[String, InstructionLocation]) shouldBe Left("ERROR (line 1): Symbol not found ('NON_EXISTENT_LABEL')")
    }
  }

  describe("JSRR parser") {
    it("successful parse") {
      val lineMetadata = LineMetadata("DOES NOT MATTER", List("JSRR", "R0"), LineNumber(1))
      parseJsrr(lineMetadata) shouldBe Right(0x4000)
    }

    it("base register is wrong") {
      val lineMetadata = LineMetadata("DOES NOT MATTER", List("JSRR", "R8"), LineNumber(1))
      parseJsrr(lineMetadata) shouldBe Left("ERROR (line 1): Expected register but found R8")
    }
  }

  describe("JMP parser") {
    it("successful parse") {
      val lineMetadata = LineMetadata("DOES NOT MATTER", List("JMP", "R6"), LineNumber(1))
      parseJmp(lineMetadata) shouldBe Right(0xC180)
    }

    it("base register is wrong") {
      val lineMetadata = LineMetadata("DOES NOT MATTER", List("JMP", "R8"), LineNumber(1))
      parseJmp(lineMetadata) shouldBe Left("ERROR (line 1): Expected register but found R8")
    }
  }