package com.github.falvarezb

  @main
  def main(files: String*): Unit =
    val result: Either[String, Unit] = if files.length == 1 then
      new Assembler().assemble(files.head)
    else if files.length > 1 then
      new Assembler().link(files.init, files.last)
    else
      Left("invalid input")

    result match
      case Left(value) => println(value)
      case Right(_) => println(s"asm files successfully assembled")



