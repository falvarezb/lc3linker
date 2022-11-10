package com.github.falvarezb

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MainTest extends AnyFunSpec with Matchers :

  describe("program args") {
    it("missing output obj file when linking asm files") {
      main("file1.asm", "file2.asm") shouldBe Left("invalid input: output obj file not specified")
    }

    it("missing input asm files") {
      main() shouldBe Left("invalid input: input asm file/s not specified")
    }

    it("successful execution when assembling a single file") {
      main("src/test/resources/abs.asm") shouldBe Right(())
    }

    it("successful execution when linking multiple files") {
      main(List("src/test/resources/day_week.asm", "src/test/resources/ascii_to_binary_routine.asm","src/test/resources/day_week.obj"):_*) shouldBe Right(())
    }
  }


