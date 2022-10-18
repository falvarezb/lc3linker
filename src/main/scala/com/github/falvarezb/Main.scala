package com.github.falvarezb

  @main
  def main(asmFile: String): Unit =
    new Assembler().assemble(asmFile) match
      case Left(value) => println(value)
      case Right(_) => println(s"$asmFile compiled")



