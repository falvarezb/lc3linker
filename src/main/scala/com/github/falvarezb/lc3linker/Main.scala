package com.github.falvarezb.lc3linker

import com.github.falvarezb.lc3linker.Assembler

@main
def main(files: String*): Either[String, Unit] =
  val result: Either[String, Unit] = files.length match
    case 1 => new Assembler().assemble(files.head)
    case n if n > 1 =>
      if files.last.split('.').last == "obj" then new Assembler().link(files.init, files.last)
      else Left("invalid input: output obj file not specified")
    case _ => Left("invalid input: input asm file/s not specified")

  result match
    case Left(err) => println(err)
    case Right(_) => println(s"asm files successfully assembled")
  result



