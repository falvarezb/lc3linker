package com.github.falvarezb.lc3linker

import com.github.falvarezb.lc3linker.Assembler

@main
  def main(files: String*): Either[String, Unit] =
    val result: Either[String, Unit] = if files.length == 1 then
      new Assembler().assemble(files.head)
    else if files.length > 1 then
      if files.last.split('.').last == "obj" then
        new Assembler().link(files.init, files.last)
      else
        Left("invalid input: output obj file not specified")
    else
      Left("invalid input: input asm file/s not specified")

    result match
      case Left(err) => println(err)
      case Right(_) => println(s"asm files successfully assembled")
    result



