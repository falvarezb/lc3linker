val scala3Version = "3.1.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "lc3linker",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % "test"
  )
