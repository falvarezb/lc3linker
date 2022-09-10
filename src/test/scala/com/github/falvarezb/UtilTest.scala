package com.github.falvarezb

import com.github.falvarezb.Util.interpretEscapeSequence
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class UtilTest extends AnyFunSpec with Matchers:

  describe("interpret characters") {
    it("valid escape character is replaced") {
      interpretEscapeSequence("hi\\nbye") shouldBe "hi\nbye"
    }

    it("multiple valid escape character are replaced") {
      interpretEscapeSequence("hi\\nbye\\\"he\\ello\\\"") shouldBe "hi\nbye\"he\u001bllo\""
    }
  }


