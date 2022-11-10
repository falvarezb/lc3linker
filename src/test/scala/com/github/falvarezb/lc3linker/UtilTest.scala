package com.github.falvarezb.lc3linker

import Util.interpretEscapeSequence
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class UtilTest extends AnyFunSpec with Matchers :

  describe("interpret characters") {
    it("valid escape sequence is replaced") {
      interpretEscapeSequence("hi\\nbye")(using LineMetadata("", Nil, LineNumber(1), "")) shouldBe Right("hi\nbye\u0000")
    }

    it("multiple valid escape sequences are replaced") {
      interpretEscapeSequence("\\a\\b\\f\\r\\v hi\\nbye\\\"he\\ello\\\"")(using LineMetadata("", Nil, LineNumber(1), ""))  shouldBe Right("\u0007\b\f\r\u000b hi\nbye\"he\u001bllo\"\u0000")
    }
  }


