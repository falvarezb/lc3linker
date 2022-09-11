package com.github.falvarezb

import com.github.falvarezb.Util.interpretEscapeSequence
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class UtilTest extends AnyFunSpec with Matchers:

  describe("interpret characters") {
    it("valid escape sequence is replaced") {
      interpretEscapeSequence("hi\\nbye", LineNumber(1)) shouldBe Right("hi\nbye")
    }

    it("multiple valid escape sequences are replaced") {
      interpretEscapeSequence("hi\\nbye\\\"he\\ello\\\"", LineNumber(1)) shouldBe Right("hi\nbye\"he\u001bllo\"")
    }

    it("invalid escape sequence fails") {
      interpretEscapeSequence("hi\\gbye", LineNumber(1)) shouldBe Left("ERROR (line 1): Unrecognised escape sequence ('hi\\gbye')")
    }
  }

