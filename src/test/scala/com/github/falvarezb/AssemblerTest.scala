package com.github.falvarezb

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.io.FileInputStream
import java.util
import scala.io.Source

class AssemblerTest extends AnyFunSuite with Matchers:

  def runTest(asmFileName: String): Unit =
    val path = "src/test/resources"
    Assembler.assemble(s"$path/$asmFileName") shouldBe Right(())

    val asmFileNameWithoutExtension = asmFileName.split('.')(0)
    val expectedFile = new FileInputStream(s"$path/$asmFileNameWithoutExtension.expected.obj")
    val actualFile = new FileInputStream(s"$path/$asmFileNameWithoutExtension.obj")

    util.Arrays.compare(expectedFile.readAllBytes(), actualFile.readAllBytes()) shouldBe 0
    expectedFile.close()
    actualFile.close()

  test("t1: assembly file without labels") {
    runTest("t1.asm")
  }

  test("t2: assembly file with labels") {
    runTest("t2.asm")
  }


