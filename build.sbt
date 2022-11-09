import Dependencies._

lazy val root = project
  .in(file("."))
  .settings(
    name := "lang4",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "3.2.0",
    libraryDependencies += scalaParserCombinators,
    libraryDependencies += scalaTest % Test
  )
