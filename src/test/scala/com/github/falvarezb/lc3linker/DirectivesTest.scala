package com.github.falvarezb.lc3linker

import ControlInstructions.parseJsr
import Directives.{parseFill, parseStringz}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class DirectivesTest extends AnyFunSpec with Matchers :

  describe(".STRINGZ parser") {

    it("chars before the quotations marks") {
      given lineMetadata:LineMetadata = LineMetadata(".STRINGZ  a \"string content\"", List(".STRINGZ", "DOES NOT MATTER"), LineNumber(1), "file")
      parseStringz shouldBe Left("ERROR (file - line 1): Bad string ('.STRINGZ  a \"string content\"')")
    }

    it("chars after the quotations marks") {
      given lineMetadata:LineMetadata = LineMetadata(".STRINGZ \"string content\" a", List(".STRINGZ", "DOES NOT MATTER"), LineNumber(1), "file")
      parseStringz shouldBe Left("ERROR (file - line 1): Bad string ('.STRINGZ \"string content\" a')")
    }

    it("comment at the end of the line") {
      given lineMetadata:LineMetadata= LineMetadata(".STRINGZ \"a\" ;comment", List(".STRINGZ", "DOES NOT MATTER"), LineNumber(1), "")
      parseStringz shouldBe Right(List(0x61, 0))
    }

    it("one quotations mark missing") {
      given lineMetadata:LineMetadata = LineMetadata(".STRINGZ  a \"string content", List(".STRINGZ", "DOES NOT MATTER"), LineNumber(1), "file")
      parseStringz shouldBe Left("ERROR (file - line 1): Bad string ('.STRINGZ  a \"string content')")
    }

    it("both quotations marks missing") {
      given lineMetadata:LineMetadata = LineMetadata(".STRINGZ  a string content", List(".STRINGZ", "DOES NOT MATTER"), LineNumber(1), "file")
      parseStringz shouldBe Left("ERROR (file - line 1): Bad string ('.STRINGZ  a string content')")
    }

    it("invalid escape sequence") {
      given lineMetadata:LineMetadata = LineMetadata(".STRINGZ  \"hi\\gbye\"", List(".STRINGZ", "DOES NOT MATTER"), LineNumber(1), "file")
      parseStringz shouldBe Left("ERROR (file - line 1): Unrecognised escape sequence ('hi\\gbye')")
    }

    it("non-ASCII char") {
      given lineMetadata:LineMetadata = LineMetadata(".STRINGZ  \"d??\"", List(".STRINGZ", "DOES NOT MATTER"), LineNumber(1), "file")
      parseStringz shouldBe Left("ERROR (file - line 1): Bad string, non-ascii char ('.STRINGZ  \"d??\"')")
    }
  }

  describe(".FILL parser") {
    it("successful parse when operand is an immediate value") {
      given lineMetadata: LineMetadata = LineMetadata("DOES NOT MATTER", List(".FILL", "10"), LineNumber(1), "")
      given symbolTable: SymbolTable = Map[String, InstructionMemoryAddress]()
      parseFill shouldBe Right(10)
    }

    it("successful parse when operand is a symbolic value") {
      given lineMetadata: LineMetadata = LineMetadata("DOES NOT MATTER", List(".FILL", "LABEL"), LineNumber(1), "")
      given symbolTable: SymbolTable = Map[String, InstructionMemoryAddress]("LABEL" -> InstructionMemoryAddress(0x3003))
      parseFill shouldBe Right(0x3003)
    }

    it("immediate too big") {
      given lineMetadata: LineMetadata = LineMetadata("DOES NOT MATTER", List(".FILL", "#70000"), LineNumber(1), "file")
      given symbolTable: SymbolTable = Map[String, InstructionMemoryAddress]()
      parseFill shouldBe Left("ERROR (file - line 1): Immediate operand (#70000) out of range (-32768 to 65535)")
    }

    it("immediate too small") {
      given lineMetadata: LineMetadata = LineMetadata("DOES NOT MATTER", List(".FILL", "#-33000"), LineNumber(1), "file")
      given symbolTable: SymbolTable = Map[String, InstructionMemoryAddress]()
      parseFill shouldBe Left("ERROR (file - line 1): Immediate operand (#-33000) out of range (-32768 to 65535)")
    }
  }


