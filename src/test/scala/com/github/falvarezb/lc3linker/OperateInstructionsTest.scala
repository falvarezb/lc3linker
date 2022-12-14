package com.github.falvarezb.lc3linker

import OperateInstructions.{parseAdd, parseAnd, parseNot}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class OperateInstructionsTest extends AnyFunSpec with Matchers:
  describe("ADD parser") {
    it("successful parse when using destination register") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("ADD", "R0", "R1", "R2"), LineNumber(1), "")
      parseAdd shouldBe Right(0x1042)
    }

    it("successful parse when using immediate in decimal representation") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("ADD", "R0", "R1", "#13"), LineNumber(1), "")
      parseAdd shouldBe Right(0x106d)
    }

    it("successful parse when using immediate in default representation") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("ADD", "R0", "R1", "13"), LineNumber(1), "")
      parseAdd shouldBe Right(0x106d)
    }

    it("successful parse when using negative immediate") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("ADD", "R5", "R5", "#-1"), LineNumber(1), "")
      parseAdd shouldBe Right(0x1b7f)
    }

    it("successful parse when using immediate in hex representation") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("ADD", "R0", "R1", "xa"), LineNumber(1), "")
      parseAdd shouldBe Right(0x106a)
    }

    it("destination register DR is wrong") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("ADD", "R8", "R1", "#13"), LineNumber(1), "file")
      parseAdd shouldBe Left("ERROR (file - line 1): Expected register but found R8")
    }

    it("source register SR1 is wrong") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("ADD", "R0", "SR1", "#13"), LineNumber(1), "file")
      parseAdd shouldBe Left("ERROR (file - line 1): Expected register but found SR1")
    }

    it("decimal immediate too big") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("ADD", "R0", "R1", "#16"), LineNumber(1), "file")
      parseAdd shouldBe Left("ERROR (file - line 1): Immediate operand (#16) out of range (-16 to 15)")
    }

    it("decimal immediate too small") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("ADD", "R0", "R1", "#-17"), LineNumber(1), "file")
      parseAdd shouldBe Left("ERROR (file - line 1): Immediate operand (#-17) out of range (-16 to 15)")
    }

    it("hex immediate too big") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("ADD", "R0", "R1", "xf1"), LineNumber(1), "file")
      parseAdd shouldBe Left("ERROR (file - line 1): Immediate operand (xf1) out of range (-16 to 15)")
    }

    it("hex immediate too small") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("ADD", "R0", "R1", "x-f2"), LineNumber(1), "file")
      parseAdd shouldBe Left("ERROR (file - line 1): Immediate operand (x-f2) out of range (-16 to 15)")
    }

    it("wrong immediate") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("ADD", "R0", "R1", "#y"), LineNumber(1), "file")
      parseAdd shouldBe Left("ERROR (file - line 1): Immediate #y is not a numeric value")
    }
  }

  describe("AND parser") {
    it("successful parse when using destination register") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("AND", "R0", "R1", "R2"), LineNumber(1), "")
      parseAnd shouldBe Right(0x5042)
    }
  }

  describe("NOT parser") {
    it("successful parse") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("NOT", "R4", "R5"), LineNumber(1), "")
      parseNot shouldBe Right(0x997f)
    }

    it("destination register DR is wrong") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("NOT", "R8", "R5"), LineNumber(1), "file")
      parseNot shouldBe Left("ERROR (file - line 1): Expected register but found R8")
    }

    it("source register SR is wrong") {
      given lineMetadata:LineMetadata = LineMetadata("DOES NOT MATTER", List("NOT", "R0", "SR"), LineNumber(1), "file")
      parseNot shouldBe Left("ERROR (file - line 1): Expected register but found SR")
    }
  }
