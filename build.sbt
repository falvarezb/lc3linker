val scala3Version = "3.2.0"

lazy val commonSettings = Seq(
    name := "lc3linker",
    version := "0.1.0",
    scalaVersion := scala3Version
)

libraryDependencies ++=Seq(
    "org.scalatest" %% "scalatest" % "3.2.12" % "test",
    "org.typelevel" %% "cats-core" % "2.7.0"
)

//Compile / mainClass := Some("com.github.falvarezb.Main")
Docker / packageName := "fab/lc3linker"
dockerExposedVolumes := Seq("/data")

lazy val root = project
  .in(file("."))
  .enablePlugins(
    JavaAppPackaging,
    DockerPlugin
  )
  .settings(commonSettings: _*)

scalacOptions ++= Seq(
    "-deprecation",         // emit warning and location for usages of deprecated APIs
    "-explain",             // explain errors in more detail
    "-explain-types",       // explain type errors in more detail
    "-feature",             // emit warning and location for usages of features that should be imported explicitly
    "-indent",              // allow significant indentation.
    "-new-syntax",          // require `then` and `do` in control expressions.
    "-print-lines",         // show source code line numbers.
    "-unchecked",           // enable additional warnings where generated code depends on assumptions
    "-Ykind-projector",     // allow `*` as wildcard to be compatible with kind projector
    "-Xfatal-warnings",     // fail the compilation if there are any warnings
    "-Xmigration",          // warn about constructs whose behavior may have changed since version
    "-source:3.2"
)

//addCommandAlias("full", ";clean ;compile; coverage; test; coverageReport; scalastyle; stage")

coverageFailOnMinimum := true
coverageMinimumStmtTotal := 90
coverageMinimumBranchTotal := 90
