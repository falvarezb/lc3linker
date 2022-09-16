package com.github.falvarezb

import com.github.falvarezb.Parsers.{parseFill, parseStringz}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ParserTest extends AnyFunSpec with Matchers:

  describe(".STRINGZ parser") {

    it("chars before the quotations marks") {
      val lineMetadata = LineMetadata(".STRINGZ  a \"string content\"", List(".STRINGZ", "DOES NOT MATTER"), LineNumber(1))
      parseStringz(lineMetadata) shouldBe Left("ERROR (line 1): Bad string ('.STRINGZ  a \"string content\"')")
    }

    it("chars after the quotations marks") {
      val lineMetadata = LineMetadata(".STRINGZ \"string content\" a", List(".STRINGZ", "DOES NOT MATTER"), LineNumber(1))
      parseStringz(lineMetadata) shouldBe Left("ERROR (line 1): Bad string ('.STRINGZ \"string content\" a')")
    }

    it("one quotations mark missing") {
      val lineMetadata = LineMetadata(".STRINGZ  a \"string content", List(".STRINGZ", "DOES NOT MATTER"), LineNumber(1))
      parseStringz(lineMetadata) shouldBe Left("ERROR (line 1): Bad string ('.STRINGZ  a \"string content')")
    }

    it("both quotations marks missing") {
      val lineMetadata = LineMetadata(".STRINGZ  a string content", List(".STRINGZ", "DOES NOT MATTER"), LineNumber(1))
      parseStringz(lineMetadata) shouldBe Left("ERROR (line 1): Bad string ('.STRINGZ  a string content')")
    }

    it("invalid escape sequence") {
      val lineMetadata = LineMetadata(".STRINGZ  \"hi\\gbye\"", List(".STRINGZ", "DOES NOT MATTER"), LineNumber(1))
      parseStringz(lineMetadata) shouldBe Left("ERROR (line 1): Unrecognised escape sequence ('hi\\gbye')")
    }

    it("non-ASCII char") {
      val lineMetadata = LineMetadata(".STRINGZ  \"dπ\"", List(".STRINGZ", "DOES NOT MATTER"), LineNumber(1))
      parseStringz(lineMetadata) shouldBe Left("ERROR (line 1): Bad string, non-ascii char ('.STRINGZ  \"dπ\"')")
    }
  }

  describe(".FILL parser") {
    it("successful parse when operand is an immediate value") {
      val lineMetadata = LineMetadata("DOES NOT MATTER", List(".FILL", "10"), LineNumber(1))
      val symbolTable = Map[String, InstructionLocation]()
      parseFill(lineMetadata, symbolTable) shouldBe Right(10)
    }

    it("successful parse when operand is a symbolic value") {
      val lineMetadata = LineMetadata("DOES NOT MATTER", List(".FILL", "LABEL"), LineNumber(1))
      val symbolTable = Map[String, InstructionLocation]("LABEL" -> InstructionLocation(0x3003))
      parseFill(lineMetadata, symbolTable) shouldBe Right(0x3003)
    }

    it("immediate too big") {
      val lineMetadata = LineMetadata("DOES NOT MATTER", List(".FILL", "#70000"), LineNumber(1))
      val symbolTable = Map[String, InstructionLocation]()
      parseFill(lineMetadata, symbolTable) shouldBe Left("ERROR (line 1): Immediate operand (#70000) out of range (-32768 to 65535)")
    }

    it("immediate too small") {
      val lineMetadata = LineMetadata("DOES NOT MATTER", List(".FILL", "#-33000"), LineNumber(1))
      val symbolTable = Map[String, InstructionLocation]()
      parseFill(lineMetadata, symbolTable) shouldBe Left("ERROR (line 1): Immediate operand (#-33000) out of range (-32768 to 65535)")
    }
  }


