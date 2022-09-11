package com.github.falvarezb

import com.github.falvarezb.Parsers.parseStringz
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
  }


