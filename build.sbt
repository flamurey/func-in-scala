name := "func-in-scala"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "co.fs2" %% "fs2-core" % "2.3.0",
  "co.fs2" %% "fs2-io" % "2.3.0",
  "co.fs2" %% "fs2-reactive-streams" % "2.3.0",
  "org.typelevel" %% "kittens" % "2.1.0",


  "org.scalatest" %% "scalatest" % "3.1.1",
  "org.scalactic" %% "scalactic" % "3.1.1"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)